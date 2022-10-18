-module(tp_evm).
-export([q/4, q_raw/4, estimate_gas/4]).
-export([local_deploy/3, local_run/4]).

%%% tp_evm:local_deploy(<<159,255,224,0,0,0,0,1>>,hex:decode(Hex),#{})
%%% tp_evm:local_run(<<159,255,224,0,0,0,0,1>>,<<"registerProvider(string,string)">>,[<<"url1">>,<<"url2">>],#{})
estimate_gas(Node, Address, Value, ABI) ->
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

  State0 = #{ sload=>SLoad,
              gas=>Gas,
              data=>#{
                      address=>binary:decode_unsigned(Address),
                      callvalue => Value,
                      caller => 1024,
                      origin => 1024
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
                      }
            },
  Res=try
        eevm:eval(Bytecode,#{},State0)
      catch Ec:Ee:Stack ->
              {error, iolist_to_binary(io_lib:format("~p:~p@~p",[Ec,Ee,hd(Stack)]))}
      end,
  gun:close(ConnPid),
  case Res of
    {done, {return, Data}, #{gas:=GL}} ->
      {ok, {return, Data}, Gas-GL};
    {done, 'stop', #{gas:=GL}} ->
      {ok, stop, Gas-GL};
    {done, 'eof', #{gas:=GL}} ->
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

  State0 = #{ sload=>SLoad,
              gas=>Gas,
              data=>#{
                      address=>binary:decode_unsigned(Address),
                      caller => 1024,
                      origin => 1024
                     }
            },

  BArgs=tpapi2:evm_encode(
          lists:map(
            fun(<<"0x",Hex/binary>>) ->
                {bin, hex:decode(Hex)};
               (Int) when is_integer(Int) ->
                Int;
               (Other) when is_binary(Other) ->
                Other
            end,Args)
         ),

  IFun = fun(<<"0x",Hex:8/binary>>) ->
             B=hex:decode(Hex),
             binary:decode_unsigned(B);
            (B) when is_binary(B) ->
             <<X:32/big,_/binary>> = esha3:keccak_256(B),
             X
         end(Function),
  CallData = << IFun:32/big, BArgs/binary>>,

  Res=try
        eevm:eval(Bytecode,#{},State0#{cd=>CallData,sha3=> fun esha3:keccak_256/1})
      catch Ec:Ee:Stack ->
              {error, iolist_to_binary(io_lib:format("~p:~p@~p",[Ec,Ee,hd(Stack)]))}
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
  SLoad=fun(Addr, IKey, _Ex0) ->
            Res=ldb:get(Addr, {storage, IKey}),
            io:format("=== Load key ~s:~s => ~s~n",[naddress:encode(
                                                      binary:encode_unsigned(
                                                        Addr)
                                                     ),
                                                    hex:encode(
                                                      binary:encode_unsigned(
                                                        IKey)
                                                     ),hex:encode(binary:encode_unsigned(Res))]),
            Res
        end,
  State0 = #{
             sload=>SLoad,
             gas=>100000000,
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
                      maps:get({Addr, stor}, Xtra, #{}),
                      Stor
                     ),
               FinState#{extra=>Xtra#{{Addr, stor} => NewS}}
           end,

  GetCodeFun = fun(Addr,Ex0) ->
                   case maps:is_key({Addr,code},Ex0) of
                     true ->
                       maps:get({Addr,code},Ex0,<<>>);
                     false ->
                       io:format(".: Get code for  ~p~n",[Addr]),
                       GotCode=ldb:get(Addr, code),
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
  BeforeCall = fun(CallKind,CFrom,_Code,_Gas,
                   #{address:=CAddr, value:=V}=CallArgs,
                   #{global_acc:=GAcc}=Xtra) ->
                   io:format("EVMCall from ~p ~p: ~p~n",[CFrom,CallKind,CallArgs]),
                   if V > 0 ->
                        TX=msgpack:pack(#{
                                          "k"=>tx:encode_kind(2,generic),
                                          "to"=>binary:encode_unsigned(CAddr),
                                          "p"=>[[tx:encode_purpose(transfer),<<"SK">>,V]]
                                         }),
                        {TxID,CTX}=generate_block_process:complete_tx(TX,
                                                               binary:encode_unsigned(CFrom),
                                                               GAcc),
                        SCTX=CTX#{sigverify=>#{valid=>1},norun=>1},
                        NewGAcc=generate_block_process:try_process([{TxID,SCTX}], GAcc),
                        io:format(">><< LAST ~p~n",[maps:get(last,NewGAcc)]),
                        case maps:get(last,NewGAcc) of
                          failed ->
                            throw({cancel_call,insufficient_fund});
                          ok ->
                            ok
                        end,
                        Xtra#{global_acc=>NewGAcc};
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
                        maps:get({Addr,stor},Ex2,#{}),
                        StRet),
                  Ex3=maps:merge(Ex2,
                                 #{
                                   {Addr,stor} => St2,
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
                 BArgs=tpapi2:evm_encode(
                         lists:map(
                           fun(<<"0x",Hex/binary>>) ->
                               {bin, hex:decode(Hex)};
                              (Int) when is_integer(Int) ->
                               Int;
                              (Other) when is_binary(Other) ->
                               Other
                           end,Arg)
                        ),

                 IFun = fun(<<"0x",Hex:8/binary>>) ->
                            B=hex:decode(Hex),
                            binary:decode_unsigned(B);
                           (X) when is_integer(X) ->
                            X;
                           (B) when is_binary(B) ->
                            <<X:32/big,_/binary>> = esha3:keccak_256(B),
                            X
                        end(Fun),
                 << IFun:32/big, BArgs/binary>>;
               undefined ->
                 <<>>
             end,
  
  Ex1=#{
        la=>0,
        log=>[]
       },
  eevm:eval(Code,#{},State0#{cd=>CallData,sha3=> fun esha3:keccak_256/1,
                             extra=>Ex1,
                             sload=>SLoad,
                             finfun=>FinFun,
                             get=>#{
                                    code => GetCodeFun,
                                    balance => GetBalFun
                                   },
                             cb_beforecall => BeforeCall,
                             logger=>fun logger/4,
                             create => CreateFun,
                             trace=>whereis(eevm_tracer)
                            }).
  
local_deploy(Address, Code, _Ed) ->
  Res=run(Address, Code, #{deploy=>1}),

  FmtStack=fun(St) ->
               [<<"0x",(hex:encode(binary:encode_unsigned(X)))/binary>> || X<-St]
           end,
  case Res of
    {done, {return,RetVal}, #{extra:=X}} ->
      store(X),
      io:format("Extra ~p~n",[X]),
      ldb:put(Address,code, RetVal),
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
    {error, {bad_instruction,_}, #{stack:=St}} ->
      {error, bad_instruction, FmtStack(St)}
  end.

store(#{log:=Log}=Map) ->
  maps:fold(
    fun({Addr,stor},Val,_Acc) ->
        %io:format("Store ~p~n",[Val]),
        ldb:put(binary:encode_unsigned(Addr),storage,
                maps:to_list(Val)
               );
       (_,_,Acc) ->
        Acc
    end, [], Map),
  lists:foreach(
    fun(LE) ->
        io:format("LOG: ~p~n",[lists:map(
                               fun(E) when is_binary(E) -> binary_to_list(hex:encode(E));
                                  (E) when is_list(E) ->
                                   lists:map(
                                     fun(E1) when is_binary(E1) -> binary_to_list(hex:encode(E1));
                                         (E1) -> E1
                                     end, E);
                                  (E) -> E
                               end,
                               LE)
                              ])
    end, Log).
  

local_run(Address, Fun, Args, _Ed) ->
  Code=ldb:get(Address, code),
  Res=run(Address, Code, #{call => {Fun, Args}}),

  FmtStack=fun(St) ->
               [<<"0x",(hex:encode(binary:encode_unsigned(X)))/binary>> || X<-St]
           end,
  case Res of
    {done, {return,RetVal}, #{stack:=_St, extra:=X}} ->
      store(X),
      {return, binary:decode_unsigned(RetVal)};
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

logger(Message,LArgs0,#{log:=PreLog}=Xtra,#{data:=#{address:=A,caller:=O}}=_EEvmState) ->
  LArgs=[binary:encode_unsigned(I) || I <- LArgs0],
  %?LOG_INFO("EVM log ~p ~p",[Message,LArgs]),
  %io:format("==>> EVM log ~p ~p~n",[Message,LArgs]),
  maps:put(log,[([evm,binary:encode_unsigned(A),binary:encode_unsigned(O),Message,LArgs])|PreLog],Xtra).

