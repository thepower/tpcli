-module(tp_evm).
-export([q/4, q_raw/4, estimate_gas/5, estimate_gas/4, make_calldata/2]).
-export([local_deploy/4, local_deploy/5, local_run/4]).
-export([local_redeploy/1]).
-export([decode_res/3]).

%%% tp_evm:local_deploy(<<159,255,224,0,0,0,0,1>>,hex:decode(Hex),#{})
%%% tp_evm:local_run(<<159,255,224,0,0,0,0,1>>,<<"registerProvider(string,string)">>,[<<"url1">>,<<"url2">>],#{})
estimate_gas(Node, Address, Value, ABI) ->
  estimate_gas(Node, Address, Value, ABI, #{}).

estimate_gas(Node, Address, Value, ABI, Opts) ->
  Gas = 1000000000,
  try
  {Host, Port, COpts,_} = tpapi2:parse_url(Node),
  {ok, ConnPid} = gun:open(Host,Port,COpts),
  {ok, _} = gun:await_up(ConnPid),
  Get=fun(Endpoint) ->
          StreamRef = gun:get(ConnPid, Endpoint, []),
          {response, Fin, Code, _Headers} = gun:await(ConnPid, StreamRef),
          Body=case Fin of
                 fin -> <<>>;
                 nofin ->
                   {ok, Body2} = gun:await_body(ConnPid, StreamRef),
                   Body2
               end,
          {Code, Body}
      end,

  Bytecode = case maps:get(code, Opts, undefined) of
               BCode when is_binary(BCode) ->
                 BCode;
               undefined ->
                 {200, GotBytecode} = Get(<<"/api/address/0x",(hex:encode(Address))/binary,"/code">>),
                 GotBytecode
             end,

  SLoad=fun(Addr, IKey, _Ex0) ->
            {200,St1}=Get(<<"/api/address/0x",(hex:encode(binary:encode_unsigned(Addr)))/binary,
                            "/state/0x",(hex:encode(binary:encode_unsigned(IKey)))/binary>>),
            Res=binary:decode_unsigned(St1),
            %io:format("=== Load key ~p:~p => ~p~n",[Addr,IKey,hex:encode(St1)]),
            Res
        end,

  State0 = #{ sload=>SLoad,
              gas=>Gas,

              extra => #{
                         la=>0,
                         log=>[]
                        },
              data=>#{
                      address=>binary:decode_unsigned(Address),
                      callvalue => Value,
                      caller => binary:decode_unsigned(maps:get(caller,Opts,<<1024:16/big>>)),
                      origin => binary:decode_unsigned(maps:get(origin,Opts,<<1024:16/big>>))
                     },
              cd=>ABI,
              sha3=> fun esha3:keccak_256/1,
              embedded_code => #{
                                 16#AFFFFFFFFF000000 => fun(_) ->
                                                            {1,<<0:256/big,1:256/big>>}
                                                        end
                                },
              get => #{
                       code => fun(Addr,_) ->
                                   io:format("Get code for address ~p~n",[
                                                                          naddress:encode(
                                                                            binary:encode_unsigned(Addr)
                                                                           )
                                                                         ]),
                                   throw('unsupported')
                               end
                      },
              trace=>whereis(eevm_tracer)
            },
  Res=try
        eevm:eval(Bytecode,#{},State0)
      catch EEc:EEe:EStack ->
              {error, iolist_to_binary(io_lib:format("~p:~p@~p",[EEc,EEe,hd(EStack)]))}
      end,
  gun:close(ConnPid),
  case Res of
    {done, {return, Data}, #{gas:=GL,extra:=#{log:=L}}} ->
      io:format("Log ~p~n",[L]),
      {ok, {return, Data}, Gas-GL};
    {done, 'stop', #{gas:=GL,extra:=#{log:=L}}} ->
      io:format("Log ~p~n",[L]),
      {ok, stop, Gas-GL};
    {done, 'eof', #{gas:=GL,extra:=#{log:=L}}} ->
      io:format("Log ~p~n",[L]),
      {ok, eof, Gas-GL};
    {done, 'invalid', #{gas:=GL}} ->
      {ok, invalid, Gas-GL};
    {done, {revert, Data}, #{gas:=GL}} ->
      {ok, {revert, Data}, Gas-GL};
    {error, Desc} ->
      {error, Desc, 0};
    {error, nogas, #{gas:=GL}} ->
      {error, nogas, Gas-GL};
    {error, {jump_to,_}, #{gas:=GL}} ->
      {error, bad_jump, Gas-GL};
    {error, {bad_instruction,I}, #{gas:=GL}} ->
      {error, {bad_instruction,I}, Gas-GL}
  end
  catch Ec:Ee:Stack ->
          {error, iolist_to_binary(io_lib:format("~p:~p@~p",[Ec,Ee,hd(Stack)]))}
  end.

q_raw(Node, Address, Function, Args) ->
  Gas = 1000000000,
  {Host, Port, Opts,_} = tpapi2:parse_url(Node),
  {ok, ConnPid} = gun:open(Host,Port,Opts),
  {ok, _} = gun:await_up(ConnPid),
  Get=fun(Endpoint) ->
          StreamRef = gun:get(ConnPid, Endpoint, []),
          {response, Fin, Code, _Headers} = gun:await(ConnPid, StreamRef),
          Body=case Fin of
                 fin -> <<>>;
                 nofin ->
                   {ok, Body2} = gun:await_body(ConnPid, StreamRef),
                   Body2
               end,
          {Code, Body}
      end,

  {200, Bytecode} = Get(<<"/api/address/0x",(hex:encode(Address))/binary,"/code">>),

  SLoad=fun(Addr, IKey, _Ex0) ->
            {200,St1}=Get(<<"/api/address/0x",(hex:encode(binary:encode_unsigned(Addr)))/binary,
                            "/state/0x",(hex:encode(binary:encode_unsigned(IKey)))/binary>>),
            Res=binary:decode_unsigned(St1),
            %io:format("=== Load key ~p:~p => ~p~n",[Addr,IKey,hex:encode(St1)]),
            Res
        end,

  GetCodeFun = fun(Addr,Ex0) ->
                   case maps:is_key({Addr,code},Ex0) of
                     true ->
                       maps:get({Addr,code},Ex0,<<>>);
                     false ->
                       AddrB=binary:encode_unsigned(Addr),
                       GCUrl= <<"/api/address/0x",(hex:encode(AddrB))/binary,"/code">>,
                       io:format(".: Get code for ~p (~p) : ~s~n",[Addr,AddrB,GCUrl]),
                       case Get(GCUrl) of
                         {404, _} ->
                           {ok, <<>>, maps:put({Addr,code},<<>>,Ex0)};
                         {200, GotCode} ->
                           %GotCode=ldb:get(Addr, code),
                           {ok, GotCode, maps:put({Addr,code},GotCode,Ex0)}
                       end
                   end
               end,

  State0 = #{ sload=>SLoad,
              gas=>Gas,
              get => #{
                       code => GetCodeFun
                      },
              data=>#{
                      address=>binary:decode_unsigned(Address),
                      caller => binary:decode_unsigned(<<128,1,64,4,1,0,0,213>>),
                      origin => binary:decode_unsigned(<<128,1,64,4,1,0,0,213>>)
                     }
            },
  BArgs=case contract_evm_abi:parse_signature(Function) of
          {ok,{{function,_},Sig,_}} ->
            contract_evm_abi:encode_abi(Args, Sig);
          _ ->
            tpapi2:evm_encode(
              lists:map(
                fun(<<"0x",Hex/binary>>) ->
                    {bin, hex:decode(Hex)};
                   (Int) when is_integer(Int) ->
                    Int;
                   (Other) when is_binary(Other) ->
                    Other
                end,Args)
             )
        end,
  %io:format("Args ~p~n",[BArgs]),
  hex:hexdump(BArgs),

  IFun = fun(<<"0x",Hex:8/binary>>) ->
             B=hex:decode(Hex),
             binary:decode_unsigned(B);
            (B) when is_binary(B) ->
             <<X:32/big,_/binary>> = esha3:keccak_256(B),
             X
         end(Function),
  CallData = << IFun:32/big, BArgs/binary>>,

  Res=try
        eevm:eval(Bytecode,#{},State0#{cd=>CallData,
                                       sha3=> fun esha3:keccak_256/1,
                                       trace=>whereis(eevm_tracer)})
      catch Ec:Ee:Stack ->
              {error, iolist_to_binary(io_lib:format("~p:~p@~p/~p",[Ec,Ee,hd(Stack),hd(tl(Stack))]))}
      end,
  gun:close(ConnPid),
  Res.

q(Node, Address, Function, Args) ->
  FmtStack=fun(St) ->
               [<<"0x",(hex:encode(binary:encode_unsigned(X)))/binary>> || X<-St]
           end,
  Res=q_raw(Node, Address, Function, Args),
  case Res of
    {done, {return,RetVal}, #{stack:=St}} ->
      {return, RetVal, FmtStack(St)};
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
  end.

run(Address, Code, Data) ->
  BI=fun
       (chainid, #{stack:=Stack}=BIState) ->
         BIState#{stack=>[16#c0de00000000|Stack]};
       (number,#{stack:=BIStack}=BIState) ->
         BIState#{stack=>[10+1|BIStack]};
       (timestamp,#{stack:=BIStack}=BIState) ->
         MT=os:system_time(millisecond),
         BIState#{stack=>[MT|BIStack]};
       (BIInstr,BIState) ->
         logger:error("Bad instruction ~p~n",[BIInstr]),
         {error,{bad_instruction,BIInstr},BIState}
     end,

  SLoad=fun(Addr, IKey, _Ex0) ->
            %Res=ldb:get(binary:encode_unsigned(Addr), {storage, IKey}),
            Res=ldb:storage_get(binary:encode_unsigned(Addr), IKey),
            %io:format("=== Load key ~p:~p => ~s~n",[hex:encode(binary:encode_unsigned(Addr)),
            %                                        hex:encode(
            %                                          binary:encode_unsigned(
            %                                            IKey)
            %                                         ),hex:encode(binary:encode_unsigned(Res))]),
            Res
        end,
  State0 = #{
             sload=>SLoad,
             gas=>maps:get(gas,Data,100000000),
             data=>#{
                     address=>binary:decode_unsigned(Address),
                     caller =>binary:decode_unsigned(
                                maps:get(caller, Data, Address)),
                     origin  =>binary:decode_unsigned(
                                 maps:get(caller, Data, Address))
                    }
            },

  FinFun = fun(_,_,#{data:=#{address:=Addr}, storage:=Stor, extra:=Xtra} = FinState) ->
               NewS=maps:merge(
                      maps:get({Addr, state}, Xtra, #{}),
                      Stor
                     ),
               FinState#{extra=>Xtra#{{Addr, state} => NewS}}
           end,

  GetCodeFun = fun(Addr,Ex0) ->
                   case maps:is_key({Addr,code},Ex0) of
                     true ->
                       maps:get({Addr,code},Ex0,<<>>);
                     false ->
                       io:format(".: Get LDB code for ~p~n",[binary:encode_unsigned(Addr)]),
                       GotCode=ldb:get(binary:encode_unsigned(Addr), code),
                       {ok, GotCode, maps:put({Addr,code},GotCode,Ex0)}
                   end
               end,

  GetBalFun = fun(Addr,Ex0) ->
                  case maps:is_key({Addr,value},Ex0) of
                    true ->
                      maps:get({Addr,value},Ex0);
                    false ->
                      0
                  end
              end,
  BeforeCall = fun(_CallKind,_CFrom,_Code,_Gas,
                   %#{address:=CAddr, value:=V}=CallArgs,
                   #{address:=_CAddr, value:=V}=_CallArgs,
                   %#{global_acc:=GAcc}=
                   Xtra) ->
                   %io:format("EVMCall from ~p ~p: ~p~n",[CFrom,CallKind,CallArgs]),
                   if V > 0 ->
                        %TX=msgpack:pack(#{
                        %                  "k"=>tx:encode_kind(2,generic),
                        %                  "to"=>binary:encode_unsigned(CAddr),
                        %                  "p"=>[[tx:encode_purpose(transfer),<<"SK">>,V]]
                        %                 }),
                        %{TxID,CTX}=generate_block_process:complete_tx(TX,
                        %                                       binary:encode_unsigned(CFrom),
                        %                                       GAcc),
                        %SCTX=CTX#{sigverify=>#{valid=>1},norun=>1},
                        %NewGAcc=generate_block_process:try_process([{TxID,SCTX}], GAcc),
                        %io:format(">><< LAST ~p~n",[maps:get(last,NewGAcc)]),
                        %case maps:get(last,NewGAcc) of
                        %  failed ->
                        %    throw({cancel_call,insufficient_fund});
                        %  ok ->
                        %    ok
                        %end,
                        %Xtra#{global_acc=>NewGAcc};
                        throw("cannot do it without blockchain");
                      true ->
                        Xtra
                   end
               end,

  CreateFun = fun(Value1, Code1, #{la:=Lst}=Ex0) ->
                  Addr0=naddress:construct_public(16#ffff,16#0,Lst+1),
                  io:format("Address ~p~n",[Addr0]),
                  Addr=binary:decode_unsigned(Addr0),
                  Ex1=Ex0#{la=>Lst+1},
                  %io:format("Ex1 ~p~n",[Ex1]),
                  Deploy=eevm:eval(Code1,#{},#{
                                               gas=>100000,
                                               data=>#{
                                                       address=>Addr,
                                                       callvalue=>Value1,
                                                       caller=>binary:decode_unsigned(Address),
                                                       gasprice=>1,
                                                       origin=>binary:decode_unsigned(Address)
                                                      },
                                               extra=>Ex1,
                                               sload=>SLoad,
                                               bad_instruction=>BI,
                                               finfun=>FinFun,
                                               get=>#{
                                                      code => GetCodeFun,
                                                      balance => GetBalFun
                                                     },
                                               cb_beforecall => BeforeCall,
                                               logger=>fun logger/4,
                                               trace=>whereis(eevm_tracer)
                                              }),
                  {done,{return,RX},#{storage:=StRet,extra:=Ex2}}=Deploy,
                  %io:format("Ex2 ~p~n",[Ex2]),

                  St2=maps:merge(
                        maps:get({Addr,state},Ex2,#{}),
                        StRet),
                  Ex3=maps:merge(Ex2,
                                 #{
                                   {Addr,state} => St2,
                                   {Addr,code} => RX,
                                   {Addr,value} => Value1
                                  }
                                ),
                  %io:format("Ex3 ~p~n",[Ex3]),
                  Ex4=maps:put(created,[Addr|maps:get(created,Ex3,[])],Ex3),

                  {#{ address => Addr },Ex4}
              end,

  CallData = case maps:get(call, Data, undefined) of
               {Fun, Arg} ->
                 make_calldata(Fun,Arg);

                 %BArgs=tpapi2:evm_encode(
                 %        lists:map(
                 %          fun(<<"0x",Hex/binary>>) ->
                 %              {bin, hex:decode(Hex)};
                 %             (Int) when is_integer(Int) ->
                 %              Int;
                 %             (Other) when is_binary(Other) ->
                 %              Other
                 %          end,Arg)
                 %       ),

                 %IFun = fun(<<"0x",Hex:8/binary>>) ->
                 %           B=hex:decode(Hex),
                 %           binary:decode_unsigned(B);
                 %          (X) when is_integer(X) ->
                 %           X;
                 %          (B) when is_binary(B) ->
                 %           {ok,P}=contract_evm_abi:parse_signature("symbol() returns (string)").o
                 %           <<X:32/big,_/binary>> = esha3:keccak_256(B),
                 %           X
                 %       end(Fun),
                 %<< IFun:32/big, BArgs/binary>>;
               undefined ->
                 <<>>
             end,
  
  Ex1=#{
        la=>0,
        log=>[]
       },
  eevm:eval(Code,#{},State0#{cd=>CallData,
                             sha3=> fun esha3:keccak_256/1,
                             extra=>Ex1,
                             sload=>SLoad,
                             finfun=>FinFun,
                             bad_instruction=>BI,
                             get=>#{
                                    code => GetCodeFun,
                                    balance => GetBalFun
                                   },
                             cb_beforecall => BeforeCall,
                             logger=>fun logger/4,
                             create => CreateFun,
                             trace=>whereis(eevm_tracer)
                            }).
  
local_deploy(Address, Code, Sig, Args, _Ed) ->
  ABIArgs=if Sig=/= undefined ->
             {ok,{_,In,_}}=contract_evm_abi:parse_signature(Sig),
             contract_evm_abi:encode_abi(Args,In);
           true ->
             <<>>
        end,
  local_deploy(Address, Code,ABIArgs, _Ed).

local_redeploy(Address) ->
  case ldb:get(Address,source) of
    <<>> -> throw('no_source');
    Code when is_binary(Code) ->
      DA=ldb:get(Address,deployargs),
      true=is_binary(DA),
      local_deploy(Address, Code, DA, #{})
  end.


local_deploy(Address, Code0,Arg, _Ed) ->
  Code=tp_readfile:parse_code(Code0),

  Res=run(Address, <<Code/binary,Arg/binary>>, #{deploy=>1}),

  FmtStack=fun(St) ->
               [<<"0x",(hex:encode(binary:encode_unsigned(X)))/binary>> || X<-St]
           end,
  case Res of
    {done, {return,RetVal}, #{extra:=X}=_E} ->
      store(X),
      ldb:put(Address,code, RetVal),
      ldb:put(Address,source,Code0),
      ldb:put(Address,deployargs,Arg),
      ok;
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
    {error, {bad_instruction,I}, #{stack:=St}} ->
      {error, {bad_instruction,I}, FmtStack(St)}
  end.

store(#{log:=_Log}=Map) ->
  maps:fold(
    fun({Addr,state},Val,Acc) ->
        %ldb:put(binary:encode_unsigned(Addr),storage,
        %        maps:to_list(Val)
        %       ),
        ldb:storage_put(binary:encode_unsigned(Addr),maps:to_list(Val)),
        %maps:put(hex:encode(binary:encode_unsigned(Addr)), [hex:encode(binary:encode_unsigned(U)) || U <- maps:keys(Val)], Acc);
        Acc;
       (_,_,Acc) ->
        Acc
    end, #{}, Map).
  %lists:foreach(
  %  fun(LE) ->
  %      io:format("LOG: ~p~n",[lists:map(
  %                             fun(E) when is_binary(E) -> binary_to_list(hex:encode(E));
  %                                (E) when is_list(E) ->
  %                                 lists:map(
  %                                   fun(E1) when is_binary(E1) -> binary_to_list(hex:encode(E1));
  %                                       (E1) -> E1
  %                                   end, E);
  %                                (E) -> E
  %                             end,
  %                             LE)
  %                            ])
  %  end, Log).
  

local_run(Address, Fun, Args, Ed) ->
  Code=ldb:get(Address, code),
  Res=run(Address, Code, Ed#{call => {Fun, Args}}),

  FmtStack=fun(St) ->
               [<<"0x",(hex:encode(binary:encode_unsigned(X)))/binary>> || X<-St]
           end,
  FmtLog=fun(Logs) ->
             lists:foldl(
               fun(E,A) ->
                   [E|A]
               end,[], Logs)
         end,
  case Res of
    {done, {return,RetVal}, #{stack:=St, extra:=X=#{log:=Log}}} ->
      Storage=store(X),
      case contract_evm_abi:parse_signature(Fun) of
        {ok,{{function,_},_Sig, undefined}} ->
          #{result => return,
            bin => RetVal,
            log => FmtLog(Log),
            stack => FmtStack(St),
            affected_keys => Storage
           };
        {ok,{{function,_},_Sig, RetABI}} when is_list(RetABI) ->
          try
            D=contract_evm_abi:decode_abi(RetVal,RetABI,[],fun decode_res/3),
            #{result => return,
              bin => RetVal,
              decode => case D of
                          [{_,[{<<>>,_}|_]}] -> contract_evm_abi:unwrap(D);
                          [{_,[{_,_}|_]}] -> D;
                          [{<<>>,_}|_] -> contract_evm_abi:unwrap(D);
                          _ -> D
                        end,
              log => FmtLog(Log),
              stack => FmtStack(St),
              affected_keys => Storage
             }
          catch _Ec:_Ee:S ->
                  #{result => return,
                    bin => RetVal,
                    fallback_err => [_Ec,_Ee,S],
                    log => FmtLog(Log),
                    stack => FmtStack(St),
                    affected_keys => Storage
                   }
          end
      end;
    {done, 'stop',  #{stack:=St, extra:=X=#{log:=Log}}} ->
      Storage=store(X),
      #{
        result => stop,
        log => FmtLog(Log),
        stack => FmtStack(St),
        affected_keys => Storage
       };
    {done, 'eof', #{stack:=St, extra:=X=#{log:=Log}}} ->
      Storage=store(X),
      #{
        result => eof,
        log => FmtLog(Log),
        stack => FmtStack(St),
        affected_keys => Storage
       };
    {done, 'invalid',  #{stack:=St}} ->
      #{ result => invalid,
         stack => FmtStack(St)
       };
    {done, {revert, <<8,195,121,160,Data/binary>> = Bin},  #{stack:=St, extra:=#{log:=Log}}} ->
      #{ result => revert,
         bin => Bin,
         signature => <<"Error(string)">>,
         log => FmtLog(Log),
         decode => contract_evm_abi:unwrap(contract_evm_abi:decode_abi(Data,[{<<"Error">>,string}])),
         stack => FmtStack(St)
       };
    {done, {revert, <<78,72,123,113,Data/binary>> =Bin},  #{stack:=St, extra:=#{log:=Log}}} ->
      #{ result => revert,
         bin => Bin,
         signature => <<"Panic(uint256)">>,
         log => FmtLog(Log),
         decode => contract_evm_abi:unwrap(contract_evm_abi:decode_abi(Data,[{<<"Panic">>,uint256}])),
         stack => FmtStack(St)
       };
    {done, {revert, Data},  #{stack:=St, extra:=#{log:=Log}}} ->
      #{ result => revert,
         bin => Data,
         log => FmtLog(Log),
         stack => FmtStack(St)
       };
    {error, Desc} ->
      #{ result => error,
         error => Desc
       };
    {error, nogas, #{stack:=St}} ->
      #{ result => error,
         error => nogas,
         stack => FmtStack(St)
       };
    {error, {jump_to,_}, #{stack:=St}} ->
      #{ result => error,
         error => bad_jump,
         stack => FmtStack(St)
       };
    {error, {bad_instruction,I}, #{stack:=St}} ->
      #{ result => error,
         error => bad_instruction,
         data => list_to_binary([io_lib:format("~p",[I])]),
         stack => FmtStack(St)
       }
  end.

logger(Message,LArgs0,#{log:=PreLog}=Xtra,#{data:=#{address:=A,caller:=O}}=_EEvmState) ->
  LArgs=[binary:encode_unsigned(I) || I <- LArgs0],
  %?LOG_INFO("EVM log ~p ~p",[Message,LArgs]),
  %io:format("==>> EVM log ~p ~p~n",[Message,LArgs]),
  maps:put(log,[([evm,binary:encode_unsigned(A),binary:encode_unsigned(O),Message,LArgs])|PreLog],Xtra).

make_calldata(Fun, Arg) ->
  {ok,{{function,_},FABI,_}=S} = contract_evm_abi:parse_signature(Fun),
  if(length(FABI)==length(Arg)) -> ok;
    true -> throw("count of arguments does not match with signature")
  end,
  BArgs=contract_evm_abi:encode_abi(Arg,FABI),
  X=contract_evm_abi:sig32(contract_evm_abi:mk_sig(S)),
  <<X:32/big,BArgs/binary>>.

decode_res(_,address,0) ->
  <<"0x">>;
decode_res(_,address,V) when is_integer(V) ->
  <<"0x",(hex:encode(binary:encode_unsigned(V)))/binary>>;
decode_res(_,address,V) ->
  <<"0x",(hex:encode(V))/binary>>;
decode_res(_,bytes,V) ->
  <<"0x",(hex:encode(V))/binary>>;
decode_res(_,bytes32,V) ->
  <<"0x",(hex:encode(V))/binary>>;
decode_res(_,bytes4,V) ->
  <<"0x",(hex:encode(V))/binary>>;
decode_res(<<"x_e18_",_/binary>>,uint256,V)  ->
  Decimal=integer_to_binary(V),
  Cut=size(Decimal)-18,
  if(Cut>0) ->
      <<Num:Cut/binary,Frac/binary>> = Decimal,
      <<Num/binary,".",Frac/binary>>;
    true ->
      Decimal
  end;

decode_res(_,uint256,V) when is_integer(V) ->
  if(V>72057594037927936) ->
      integer_to_binary(V);
    true ->
      V
  end;
decode_res(_,_Kind,V) ->
  V.


