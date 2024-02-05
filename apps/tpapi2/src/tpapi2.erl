-module(tpapi2).

-export([submit_tx/2,
         parse_url/1,
         ping/1,
         reg/2,
         wait_tx/3,
         evm_encode/1,
         settings/1,
         settings/2,
         ledger/2,
         get_seq/2,
         get_log/2,
         code/2,
         match_cur/2,
         httpget/2
        ]).
-export([
         gas_price/2,
         fee_price/2
        ]).
-export([
         connect/1,
         do_get/2
        ]).

connect(Node) ->
  {Host, Port, Opts,_} = parse_url(Node),
  {ok, ConnPid} = gun:open(Host,Port,Opts#{retry=>0}),
  case gun:await_up(ConnPid) of
    {ok, _} ->
      {ok, ConnPid};
    {error,Other} ->
      throw(Other)
  end.

do_get(ConnPid, Endpoint) ->
do_get1(ConnPid, Endpoint).

do_get1(ConnPid, Endpoint) ->
  StreamRef = gun:get(ConnPid, Endpoint, []),
  {response, Fin, Code, Headers} = gun:await(ConnPid, StreamRef),
  if(Code == 200) ->
      Body=case Fin of
             fin -> <<>>;
             nofin ->
               {ok, Body2} = gun:await_body(ConnPid, StreamRef),
               Body2
           end,
      {Code, Headers, Body};
    true ->
      {Code, Headers, <<>>}
  end.

submit_tx(Node, Tx) ->
  submit_tx(Node, Tx, []).

submit_tx(Node, Tx, Opts) ->
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
              case lists:member(nowait,Opts) of
                true ->
                  gun:close(ConnPid),
                  {ok,TxID};
                false ->
                  wait_tx(ConnPid, TxID, erlang:system_time(second)+30)
              end;
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
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/address/0x"++binary_to_list(hex:encode(Addr))++"/code"),
  gun:close(ConnPid),
  if Code==200 ->
       {ok, Body};
     true ->
       {error, Code}
  end.

get_log(Node, Height) ->
  {ok, ConnPid} = connect(Node),
  %http://c1n2.thepower.io:1081/api/logs_height/2786
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/logs_height/"++integer_to_list(Height)++".mp?bin=raw"),
  gun:close(ConnPid),
  if Code==200 ->
       {ok,M}=msgpack:unpack(Body),
       case M of
         #{<<"ok">> := true, <<"log">> := #{<<"logs">>:=Data}} ->
           {ok,lists:map(
                 fun(D) ->
                     {ok, DD} = msgpack:unpack(D),
                     DD
                 end, Data)};
         Any ->
           logger:notice("Got unexpected result ~p",[Any]),
           {error, decode}
       end;
     true ->
       {error, Code}
  end.


get_seq(Node, Addr) ->
  {ok, ConnPid} = connect(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/address/0x"++binary_to_list(hex:encode(Addr))++".mp?bin=raw"),
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

httpget(Node, Path) ->
  {ok, ConnPid} = connect(Node),
  {Code, Header, Body} = do_get1(ConnPid,Path),
  gun:close(ConnPid),
  if Code==200 ->
       case proplists:get_value(<<"content-type">>, Header, <<"application/octet-stream">>) of
         <<"application/json">> ->
           jsx:decode(Body, [return_maps]);
         <<"application/msgpack">> ->
           {ok,M}=msgpack:unpack(Body),
           M;
         _ ->
           Body
       end;
     true ->
       {error, Code}
  end.

ledger(Node, Addr) ->
  {ok, ConnPid} = connect(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/address/0x"++binary_to_list(hex:encode(Addr))++".mp?bin=raw"),
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
  settings(Node,[]).

settings(Node, Path) ->
  {ok, ConnPid} = connect(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/settings.mp"),
  gun:close(ConnPid),
  if Code==200 ->
       {ok,M}=msgpack:unpack(Body),
       case M of
         #{<<"ok">> := true, <<"settings">> := Sets} ->
           {ok, take_settings(Path,Sets)};
         _ ->
           throw('cant_decode')
       end;
     true ->
       {error, Code}
  end.

take_settings(Path,Sets) when is_list(Path) ->
  settings:get(Path,Sets);

take_settings(Paths,Sets) when is_map(Paths) ->
  maps:map(
    fun(_,Path) ->
        settings:get(Path,Sets)
    end, Paths).

ping(Node) ->
  {ok, ConnPid} = connect(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/status"),
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
                  case get(warned) of
                    undefined ->
                      logger:notice("-=-=-= [ connection is insecure ] =-=-=-~n",[]),
                      put(warned, true);
                    _ ->
                      ok
                  end,
                  {
                   #{ transport=>tls,
                     transport_opts => [
                                        {versions,['tlsv1.2']},
                                        {verify,verify_none}
                                       ]
                   },
                  maps:get(port,P,443)
                 };
                "https" -> {
                  #{ transport=>tls,
                     transport_opts => [{verify, verify_peer},
                                        {customize_hostname_check, CHC},
                                        {depth, 5},
                                        {versions,['tlsv1.2']},
                                        %{versions,['tlsv1.3']},
                                        {cacerts, CaCerts}
                                       ]
                   },
                  maps:get(port,P,443)
                 };
                "http" ->
                  case get(warned) of
                    undefined ->
                      logger:notice("-=-=-= [ connection is not encrypted, so insecure ] =-=-=-~n",[]),
                      put(warned, true);
                    _ ->
                      ok
                  end,
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

fee_price(Bytes,Host) ->
  {ok,R}=tpapi2:settings(Host),
  Root=maps:get(<<"fee">>,maps:get(<<"current">>,R,#{}),#{}),
  case maps:is_key(<<"params">>, Root) of
    false ->
      #{};
    true ->
      maps:fold(
        fun(Cur,#{<<"base">>:=Base,<<"kb">>:=KB}=Rate,A) ->
            BaseEx=maps:get(<<"baseextra">>, Rate, 0),
            BodySize=Bytes-32,
            ExtCur=max(0, BodySize-BaseEx),
            maps:put(Cur,Base+trunc(ExtCur*KB/1024),A);
           (_,_,A) ->
            A
        end,
        #{},
        Root)
  end.

gas_price(N,Host) ->

  {ok,R}=tpapi2:settings(Host),
  Gas0=maps:get(<<"gas">>,maps:get(<<"current">>,R,#{}),#{}),
  maps:map(
    fun(_,V0) ->
        V=case V0 of
            #{<<"gas">> := Nom,<<"tokens">> := Den} ->
              Nom/Den;
            Nom when is_integer(Nom) ->
              Nom
          end,
        ceil(N/V)
    end, Gas0).

match_cur(FeePrice, Amounts) ->
  case maps:size(FeePrice) == 0 of
    true ->
      undefined;
    false ->
      {DC,DAm}=hd(maps:to_list(FeePrice)),
      maps:fold(
        fun(Cur,Amount,Acc) ->
            CAm=maps:get(Cur,Amounts,0),
            if CAm>=Amount ->
                 {Amount, Cur};
               true ->
                 Acc
            end
        end,
        {DAm, DC},
        FeePrice)
  end.

