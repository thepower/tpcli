-module(tp_evm).
-export([q/4]).

q(Node, Address, Function, Args) ->
  Gas = 10000000,
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
  FmtStack=fun(St) ->
               [<<"0x",(hex:encode(binary:encode_unsigned(X)))/binary>> || X<-St]
           end,
  gun:close(ConnPid),
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


