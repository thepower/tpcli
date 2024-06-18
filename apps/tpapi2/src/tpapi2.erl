-module(tpapi2).

-export([submit_tx/2,
         submit_tx/3,
         parse_url/1,
         ping/1,
         reg/2,
         reg/3,
         wait_tx/3,
         evm_encode/1,
         settings/1,
         settings/2,
         ledger/2,
         get_seq/2,
         get_log/2,
         code/2,
         state/3,
         match_cur/2,
         httpget/2,
         block/2,
         logs/2,
         evm_run/6,
         nodestatus/1,
         sort_peers/1,
         get_lstore/3
        ]).
-export([
         gas_price/2,
         fee_price/2
        ]).
-export([
         connect/1,
         do_get/2
        ]).

-export([search_tx_by_txid/2]).

decode_txid(TxId) when is_binary(TxId) ->
  case binary:split(TxId, <<"-">>) of
    [N0, N1] ->
      Bin = base58:decode(N0),
      {ok, N1, decode_ints(Bin)};
    _ ->
      {error, invalid_tx_id}
  end.

decode_int(<<0:1/big,X:7/big,Rest/binary>>) ->
  {X,Rest};
decode_int(<<2:2/big,X:14/big,Rest/binary>>) ->
  {X,Rest};
decode_int(<<6:3/big,X:29/big,Rest/binary>>) ->
  {X,Rest};
decode_int(<<14:4/big,X:60/big,Rest/binary>>) ->
  {X,Rest};
decode_int(<<15:4/big,S:4/big,BX:S/binary,Rest/binary>>) ->
  {binary:decode_unsigned(BX),Rest}.

%% ------------------------------------------------------------------

decode_ints(Bin) ->
  case decode_int(Bin) of
    {Int, <<>>} ->
      [Int];
    {Int, Rest} ->
     [Int|decode_ints(Rest)]
  end.

search_tx_by_txid(Node, TxID) ->
  {ok,_OriginNode,[ChainID,Height,Timestamp]} = decode_txid(TxID),
  ConnPid = connect_or_reuse(Node),
  {ok,Status}=nodestatus(ConnPid),
  NodeChain=maps:get(<<"chain">>,maps:get(<<"blockchain">>,Status)),
  if NodeChain =/= ChainID ->
       fin_or_keep(Node,ConnPid),
       {error,wrong_chain};
     true ->
       case do_get(ConnPid, "/api/logs_height/"++integer_to_list(Height)) of
         {200, _Hdr, Body} ->
           case jsx:decode(Body,[return_maps]) of
             #{<<"ok">> := true,<<"log">>:=#{<<"blkid">>:=BlkID}} ->
               R=try
                   iterate_blocks(ConnPid, BlkID, TxID, Timestamp div 1000000)
                 catch Ec:Ee ->
                         {error, {Ec,Ee}}
                 end,
               fin_or_keep(Node,ConnPid),
               R;
             Any ->
               fin_or_keep(Node,ConnPid),
               {error, {unexpected_response,Any}}
           end;
         {Code,_,_Body} ->
           fin_or_keep(Node,ConnPid),
           {error, Code}
       end
  end.

iterate_blocks(ConnPid, BlkID, TxID, TxTime) ->
  logger:debug("Getting /api/blockinfo/~s",[BlkID]),
  case do_get(ConnPid, "/api/blockinfo/"++binary_to_list(BlkID)) of
    {200, _Hdr, Body} ->
      case jsx:decode(Body,[return_maps]) of
        #{<<"ok">> := true,
          <<"block">>:=#{
           <<"header">>:=#{
            <<"roots">>:=#{
             <<"mean_time">>:=HexBlockTime
            }
           },
           <<"txs_ids">>:=Success,
           <<"failed">>:=Failed
          }=Blk} ->
          case lists:member(TxID,Success) of
            true ->
              {found, {success, BlkID}};
            false when is_map(Failed) ->
              case maps:is_key(TxID,Failed) of
                true ->
                  {found, {failed, BlkID}};
                false ->
                  BlockTime=binary:decode_unsigned(hex:decode(HexBlockTime)),
                  case maps:is_key(<<"child">>,Blk) of
                    false ->
                      not_found;
                    true when BlockTime-TxTime>900000 ->
                      not_found;
                    true ->
                      iterate_blocks(ConnPid, maps:get(<<"child">>,Blk), TxID, TxTime)
                  end
              end;
            false ->
              BlockTime=binary:decode_unsigned(hex:decode(HexBlockTime)),
              case maps:is_key(<<"child">>,Blk) of
                false ->
                  not_found;
                true when BlockTime-TxTime>900000 ->
                  not_found;
                true ->
                  iterate_blocks(ConnPid, maps:get(<<"child">>,Blk), TxID, TxTime)
              end
          end;
        Other ->
          {error, {unexpected_response,Other}}
      end;
    {Code,_Hdr,_Body} ->
      {error, Code}
  end.

nodestatus(Node) ->
  ConnPid = connect_or_reuse(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,
                              "/api/node/status"
                             ),
  fin_or_keep(Node,ConnPid),
  if Code==200 ->
       case jsx:decode(Body,[return_maps]) of
         #{<<"ok">> := true,<<"status">>:=S} ->
           {ok, S};
         Any ->
           {error, Any}
       end;
     true ->
       {error, Code}
  end.

connect(Node) ->
  {Host, Port, Opts,_} = parse_url(Node),
  {ok, ConnPid} = gun:open(Host,Port,Opts#{retry=>0}),
  case gun:await_up(ConnPid) of
    {ok, _} ->
      {ok, ConnPid};
    {error,Other} ->
      throw(Other)
  end.

connect_or_reuse(Node) when is_pid(Node) ->
  Node;
connect_or_reuse(Node) when is_binary(Node) orelse is_list(Node) ->
  {ok, ConnPid} = connect(Node),
  ConnPid.

fin_or_keep(Node, Pid) ->
  if Node == Pid ->
       ok;
     true ->
       gun:close(Pid)
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
  ConnPid=connect_or_reuse(Node),
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

  Res0=Post("/api/tx/new.bin",
            if is_map(Tx) ->
                 tx:pack(Tx);
               is_binary(Tx) ->
                 Tx
            end),
  case Res0 of
    {200, JSON} ->
      case jsx:decode(JSON, [return_maps]) of
        #{<<"ok">>:=true,
          <<"result">>:= <<"ok">>,
          <<"txid">> := TxID
         } ->
          case lists:member(nowait,Opts) of
            true ->
              fin_or_keep(Node, ConnPid),
              {ok,TxID};
            false ->
              R=wait_tx(ConnPid, TxID, erlang:system_time(second)+30),
              fin_or_keep(Node, ConnPid),
              R
          end;
        _ ->
          fin_or_keep(Node, ConnPid),
          throw('bad_result')
      end;
    {Code1, JSON} when Code1 >= 400 ->
      fin_or_keep(Node, ConnPid),
      {error, jsx:decode(JSON, [return_maps])}
  end.

wait_tx(ConnPid0, TxID, Timeout) ->
  ConnPid=connect_or_reuse(ConnPid0),
  Now=erlang:system_time(second),
  if (Now>Timeout) ->
       fin_or_keep(ConnPid0, ConnPid),
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
               fin_or_keep(ConnPid0, ConnPid),
               {ok, Result#{<<"txid">> => TxID}};
             Other ->
               fin_or_keep(ConnPid0, ConnPid),
               {error, Other}
           end;
         true ->
           fin_or_keep(ConnPid0, ConnPid),
           {error, bad_res}
       end
  end.

block(Node, Hash) ->
  ConnPid = connect_or_reuse(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/binblock/"++binary_to_list(hex:encode(Hash))),
  fin_or_keep(Node,ConnPid),
  if Code==200 ->
       {ok, block:unpack(Body)};
     true ->
       {error, Code}
  end.

logs(Node, Hash) ->
  ConnPid = connect_or_reuse(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/logs/"++binary_to_list(hex:encode(Hash))++".mp"),
  fin_or_keep(Node,ConnPid),
  if Code==200 ->
       {ok, #{<<"ok">> := true, <<"log">>:= Dec}} = msgpack:unpack(Body),
       {ok, Dec};
     true ->
       {error, Code}
  end.

code(Node, Addr) ->
  ConnPid = connect_or_reuse(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/address/0x"++binary_to_list(hex:encode(Addr))++"/code"),
  fin_or_keep(Node,ConnPid),
  if Code==200 ->
       {ok, Body};
     true ->
       {error, Code}
  end.

state(Node, Addr, Key) ->
  ConnPid = connect_or_reuse(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,
                              "/api/address/0x"++binary_to_list(hex:encode(Addr))++
                              "/state/0x"++binary_to_list(hex:encode(Key))
                             ),
  fin_or_keep(Node,ConnPid),
  if Code==200 ->
       {ok, Body};
     true ->
       {error, Code}
  end.

get_log(Node, Height) ->
  ConnPid = connect_or_reuse(Node),
  %http://c1n2.thepower.io:1081/api/logs_height/2786
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/logs_height/"++integer_to_list(Height)++".mp?bin=raw"),
  fin_or_keep(Node,ConnPid),
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


is_printable(<<>>) ->
  true;
is_printable(<<B0,Bin/binary>>) when B0>=32 andalso B0<127 ->
  is_printable(Bin);
is_printable(_) ->
  false.

get_lstore(Node, Addr, Path) ->
  ConnPid = connect_or_reuse(Node),
  TPath=lists:foldr(fun(Component, Acc) when is_binary(Component) ->
                        case is_printable(Component) of
                          false ->
                            ["/0x",hex:encode(Component)|Acc];
                          true ->
                            ["/",Component|Acc]
                        end;
                       (Component, Acc) ->
                        ["/",Component|Acc]
                    end,
                    [],
                    Path),
  QS="/api/address/0x"++binary_to_list(hex:encode(Addr))++
     "/lstore"++binary_to_list(list_to_binary(TPath))++".mp?bin=raw",
     io:format("QS ~p~n",[QS]),
  {Code, _Hdr, Body} = do_get(ConnPid,QS),
  fin_or_keep(Node,ConnPid),
  if Code==200 ->
       %temporary fix for tpnode < v0.16
       CT=hd(lists:reverse(
               lists:filter(
                 fun({K,_}) ->
                     K==<<"content-type">>
                 end,_Hdr)
              )
            ),
       case CT of
         {<<"content-type">>,<<"application/msgpack">>} ->
           {ok, M}=msgpack:unpack(Body),
           {ok, M};
         _ ->
           {ok, Body}
       end;
     true ->
       {error, Code}
  end.

get_seq(Node, Addr) ->
  ConnPid = connect_or_reuse(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/address/0x"++binary_to_list(hex:encode(Addr))++".mp?bin=raw"),
  fin_or_keep(Node,ConnPid),
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
  ConnPid = connect_or_reuse(Node),
  {Code, Header, Body} = do_get1(ConnPid,Path),
  fin_or_keep(Node,ConnPid),
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
  ConnPid = connect_or_reuse(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/address/0x"++binary_to_list(hex:encode(Addr))++".mp?bin=raw"),
  fin_or_keep(Node,ConnPid),
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
  ConnPid = connect_or_reuse(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/settings.mp"),
  fin_or_keep(Node, ConnPid),
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
  ConnPid = connect_or_reuse(Node),
  {Code, _Hdr, Body} = do_get(ConnPid,"/api/status"),
  fin_or_keep(Node,ConnPid),
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
  reg(Node, Priv, []).

reg(Node, Priv, Opts) ->
  Pub=tpecdsa:calc_pub(Priv),
  Tx=tx:sign(
       tx:construct_tx(
         #{kind=>register,
           t => os:system_time(millisecond),
           ver=>2,
           keys=>[Pub]
          }),Priv),
  submit_tx(Node,Tx,Opts).

evm_run(Node, Address, Function, Params, ABI, Gas) ->
  case erlang:function_exported(eevm,eval,3) of
    false ->
      throw('no_eevm');
    true -> ok
  end,
  ConnPid = connect_or_reuse(Node),

  case do_get1(ConnPid, <<"/api/address/0x",(hex:encode(Address))/binary,"/code">>) of
    {200, _Hdr, Bytecode} ->
      %Bytecode = eevm_asm:assemble(<<"push 0 \nsload \npush 0 \nmstore \ncalldatasize \npush 0 \npush 32 \ncalldatacopy \npush 24 \ncalldatasize \npush 32 \nadd \nsub \npush 24\nreturn">>),
      SLoad=fun(Addr, IKey, _Ex0) ->
                case do_get1(ConnPid, <<"/api/address/0x",(hex:encode(binary:encode_unsigned(Addr)))/binary,
                           "/state/0x",(hex:encode(binary:encode_unsigned(IKey)))/binary>>) of
                  {200,_,St1} ->
                    Res=binary:decode_unsigned(St1),
                    logger:debug("=== Load key ~p:~p => ~p",[Addr,IKey,hex:encode(St1)]),
                    Res;
                  Other ->
                    logger:notice("address ~p quering storage key ~p error: ~p",[Addr,IKey,Other]),
                    throw({bad_return,Other})
                end
            end,
      IFun = fun(B) -> {ok,E}=ksha3:hash(256, B), <<X:32/big,_/binary>> = E,X end(Function),
      {AbiIn, AbiOut} = if is_list(ABI) ->
                             case tp_abi:find_function(Function,ABI) of
                               [{{function,_},Input,Output}] ->
                                 {Input,Output};
                               _ ->
                                 {undefined,undefined}
                             end;
                           ABI == undefined ->
                             case tp_abi:parse_signature(Function) of
                               {ok, {{function, _}, Input, Output}} ->
                                 {Input, Output};
                               _ ->
                                 {undefined, undefined}
                             end;
                           true ->
                             {undefined,undefined}
                        end,
      State0 = #{ sload=>SLoad,
                  gas=>Gas,
                  data=>#{
                          address=>binary:decode_unsigned(Address),
                          caller => 1024,
                          origin => 1024
                         },
                  cd => if is_list(AbiIn) ->
                             ABin=tp_abi:encode_abi(Params,AbiIn),
                             <<IFun:32/big,ABin/binary>>;
                           is_list(Params) ->
                             PBin=tp_abi:encode_simple(Params),
                             << IFun:32/big, PBin/binary>>
                        end
                },
      Res=try
            Rr=eevm:eval(Bytecode,#{},State0),
            Rr
          catch Ec:Ee:Stack ->
                  {error, iolist_to_binary(io_lib:format("~p:~p@~p",[Ec,Ee,hd(Stack)]))}
          end,
      FmtStack=fun(St) ->
                   [<<"0x",(hex:encode(binary:encode_unsigned(X)))/binary>> || X<-St]
               end,
      fin_or_keep(Node, ConnPid),
      case Res of
        {done, {return,RetVal}, #{stack:=St}} ->
          Decode=case AbiOut of
                   undefined -> RetVal;
                   _ ->
                     case tp_abi:decode_abi(RetVal,AbiOut) of
                       R when is_list(R) ->
                         {decoded,R};
                       _ ->
                         RetVal
                     end
                 end,
          {return, Decode, FmtStack(St)};
        {done, 'stop',  #{stack:=St}} ->
          {stop, undefined, FmtStack(St)};
        {done, 'eof', #{stack:=St}} ->
          {eof, undefined, FmtStack(St)};
        {done, 'invalid',  #{stack:=St}} ->
          {error, invalid, FmtStack(St)};
        {done, {revert, Data},  #{stack:=St}} ->
          {revert, Data, FmtStack(St)};
        {error, Desc} ->
          {error, Desc, []};
        {error, nogas, #{stack:=St}} ->
          {error, nogas, FmtStack(St)};
        {error, {jump_to,_}, #{stack:=St}} ->
          {error, bad_jump, FmtStack(St)};
        {error, {bad_instruction,_}, #{stack:=St}} ->
          {error, bad_instruction, FmtStack(St)}
      end;
    {404, _, _} ->
      {error, code_not_found}
  end.



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

sort_peers(List) ->
  Weight=fun(Url) ->
             #{scheme:=HTTPs,host:=Hostname}=uri_string:parse(Url),
             Rank=(if HTTPs=="https" -> 1; true -> 0.8 end) *
              (case inet:parse_address(Hostname) of
                 {ok,_} when HTTPs=="https" -> 0.1;
                 {error,_} when HTTPs=="https"-> 1;
                 {ok,_} -> 1;
                 {error,_} -> 0.9
               end
              )*0.8+(rand:uniform()*0.2),
              Rank
         end,
  [ E || {E,_} <- lists:reverse(lists:keysort(2,[ {N, Weight(N)} || N <- List ])) ].
