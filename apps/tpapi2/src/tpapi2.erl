-module(tpapi2).

-export([submit_tx/2,
         parse_url/1,
         ping/1,
         reg/2,
         evm_encode/1,
         settings/1,
         ledger/2,
         get_seq/2,
         code/2
        ]).

connect(Node) ->
  {Host, Port, Opts,_} = parse_url(Node),
  {ok, ConnPid} = gun:open(Host,Port,Opts),
  case gun:await_up(ConnPid) of
    {ok, _} ->
      {ok, ConnPid};
    {error,Other} ->
      throw(Other)
  end.

do_get(ConnPid, Endpoint) ->
  StreamRef = gun:get(ConnPid, Endpoint, []),
  {response, Fin, Code, _Headers} = gun:await(ConnPid, StreamRef),
  if(Code == 200) ->
      Body=case Fin of
             fin -> <<>>;
             nofin ->
               {ok, Body2} = gun:await_body(ConnPid, StreamRef),
               Body2
           end,
      {Code, Body};
    true ->
      {Code, <<>>}
  end.

submit_tx(Node, Tx) ->
  {ok, ConnPid} = connect(Node), 
  Post=fun(Endpoint, Bin) ->
           StreamRef = gun:post(ConnPid, Endpoint, [], Bin, #{}),
           {response, Fin, Code, _Headers} = gun:await(ConnPid, StreamRef),
           Body=case Fin of
                  fin -> <<>>;
                  nofin ->
                    {ok, Body2} = gun:await_body(ConnPid, StreamRef),
                    Body2
                end,
           {Code, Body}
       end,

  Res0=Post("/api/tx/new.bin",if is_map(Tx) ->
                                         tx:pack(Tx);
                                       is_binary(Tx) ->
                                         Tx
                                    end),
  Res=case Res0 of
        {200, JSON} ->
          case jsx:decode(JSON, [return_maps]) of
            #{<<"ok">>:=true,
              <<"result">>:= <<"ok">>,
              <<"txid">> := TxID
             } ->
              wait_tx(ConnPid, TxID, erlang:system_time(second)+30);
            _ ->
              gun:close(ConnPid),
              throw('bad_result')
          end;
        {500, JSON} ->
          gun:close(ConnPid),
          {error, jsx:decode(JSON, [return_maps])}
      end,
  gun:close(ConnPid),
  Res.

wait_tx(ConnPid, TxID, Timeout) ->
  Now=erlang:system_time(second),
  if (Now>Timeout) ->
       {error, timeout};
     true ->
       Endpoint = "/api/tx/status/"++binary_to_list(TxID),
       StreamRef = gun:get(ConnPid, Endpoint, []),
       {response, Fin, Code, _Headers} = gun:await(ConnPid, StreamRef),
       if(Code == 200) ->
           Body=case Fin of
                  fin -> <<>>;
                  nofin ->
                    {ok, Body2} = gun:await_body(ConnPid, StreamRef),
                    Body2
                end,
           case jsx:decode(Body, [return_maps]) of
             #{<<"res">> := null, <<"ok">> := true} ->
               timer:sleep(1000),
               wait_tx(ConnPid, TxID, Timeout);
             #{<<"res">>:= Result, <<"ok">> := true} ->
               {ok, Result#{<<"txid">> => TxID}};
             Other ->
               {error, Other}
           end;
         true ->
           {error, bad_res}
       end
  end.

code(Node, Addr) ->
  {ok, ConnPid} = connect(Node), 
  {Code, Body} = do_get(ConnPid,"/api/address/0x"++binary_to_list(hex:encode(Addr))++"/code"),
  gun:close(ConnPid),
  if Code==200 ->
       {ok, Body};
     true ->
       {error, Code}
  end.

get_seq(Node, Addr) ->
  {ok, ConnPid} = connect(Node), 
  {Code, Body} = do_get(ConnPid,"/api/address/0x"++binary_to_list(hex:encode(Addr))++".mp?bin=raw"),
  gun:close(ConnPid),
  if Code==200 ->
       {ok,M}=msgpack:unpack(Body),
       case M of
         #{<<"ok">> := true, <<"info">> := #{<<"seq">>:=N}} ->
           {ok,N};
         #{<<"ok">> := true, <<"info">> := #{}} ->
           {ok,0};
         Any ->
           logger:notice("Got unexpected result ~p",[Any]),
           {error, decode}
       end;
     true ->
       {error, Code}
  end.


ledger(Node, Addr) ->
  {ok, ConnPid} = connect(Node), 
  {Code, Body} = do_get(ConnPid,"/api/address/0x"++binary_to_list(hex:encode(Addr))++".mp?bin=raw"),
  gun:close(ConnPid),
  if Code==200 ->
       {ok,M}=msgpack:unpack(Body),
       case M of
         #{<<"ok">> := true, <<"info">> := Data} ->
           {ok, Data};
         Any ->
           logger:notice("Got unexpected result ~p",[Any]),
           throw('cant_decode')
       end;
     true ->
       {error, Code}
  end.


settings(Node) ->
  {ok, ConnPid} = connect(Node), 
  {Code, Body} = do_get(ConnPid,"/api/settings.mp"),
  gun:close(ConnPid),
  if Code==200 ->
       {ok,M}=msgpack:unpack(Body),
       case M of
         #{<<"ok">> := true, <<"settings">> := Sets} ->
           {ok, Sets};
         _ ->
           throw('cant_decode')
       end;
     true ->
       {error, Code}
  end.

ping(Node) ->
  {ok, ConnPid} = connect(Node), 
  {Code, Body} = do_get(ConnPid,"/api/status"),
  gun:close(ConnPid),
  if Code==200 ->
       Res=jsx:decode(Body, [return_maps]),
       case maps:get(<<"ok">>, Res, false) of
         false -> false;
         _ -> true
       end;
     true ->
       false
  end.

reg(Node, Priv) ->
  Pub=tpecdsa:calc_pub(Priv,true),
  Tx=tx:sign(
       tx:construct_tx(
         #{kind=>register,
           t => os:system_time(millisecond),
           ver=>2,
           keys=>[Pub]
          }),Priv),
  submit_tx(Node,Tx).

% ======

parse_url(Node) when is_binary(Node) ->
  parse_url(binary_to_list(Node));

parse_url(Node) when is_list(Node) ->
  CaCerts = certifi:cacerts(),
  CHC=[
       {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
      ],
  #{scheme:=Sch,
    path:=Path,
    host:=Host}=P = uri_string:parse(Node),
  {Opts,Port}=case Sch of
                "httpsi" ->
                  logger:notice("-=-=-= [ connection is insecure ] =-=-=-~n",[]),
                  {
                   #{ transport=>tls,
                     transport_opts => []
                   },
                  maps:get(port,P,443)
                 };
                "https" -> {
                  #{ transport=>tls,
                     transport_opts => [{verify, verify_peer},
                                        {customize_hostname_check, CHC},
                                        {depth, 5},
                                        {cacerts, CaCerts}
                                       ]
                   },
                  maps:get(port,P,443)
                 };
                "http" ->
                  logger:notice("-=-=-= [ connection is not encrypted, so insecure ] =-=-=-~n",[]),
                  {
                   #{ transport=>tcp },
                   maps:get(port,P,80)
                  }
              end,
  {Host,
   Port,
   Opts,
   #{path=>Path}
  }.


%evm_encode_test_() ->
%  evm_encode([123,
%          <<"very_very_loooooooong_string_which_does_not_fit">>,
%          <<"little bit shorter string">>,
%          <<"short">>]).

evm_encode_str(Bin) ->
  Pad = case (size(Bin) rem 32) of
          0 -> 0;
          N -> 32 - N
        end*8,
  <<(size(Bin)):256/big,Bin/binary,0:Pad/big>>.

evm_encode(Elements) ->
  HdLen=length(Elements)*32,
  {H,B,_}=lists:foldl(
            fun(E, {Hdr,Body,BOff}) when is_integer(E) ->
                {<<Hdr/binary,E:256/big>>,
                 Body,
                 BOff};
               ({bin, Bin}, {Hdr,Body,BOff}) ->
                E=binary:decode_unsigned(Bin),
                {<<Hdr/binary,E:256/big>>,
                 Body,
                 BOff};
               (E, {Hdr,Body,BOff}) when is_binary(E) ->
                EncStr=evm_encode_str(E),
                {
                 <<Hdr/binary,BOff:256/big>>,
                 <<Body/binary,EncStr/binary>>,
                 BOff+size(EncStr)
                }
            end, {<<>>, <<>>, HdLen}, Elements),
  HdLen=size(H),
  <<H/binary,B/binary>>.


