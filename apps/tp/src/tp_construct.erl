-module(tp_construct).
-export([construct/1, transform/3]).

construct(JTX) ->
  maps:fold(fun transform/3, #{}, JTX).

transform (<<"kind">>,V,A) ->
  if(is_integer(V)) ->
      {2,Kind} = tx:decode_kind(V),
      maps:put(kind,Kind,A);
    V==<<"generic">> ->
      maps:put(kind,generic,A);
    V==<<"register">> ->
      maps:put(kind,register,A);
    V==<<"deploy">> ->
      maps:put(kind,deploy,A);
    V==<<"patch">> ->
      maps:put(kind,patch,A);
    V==<<"lstore">> ->
      maps:put(kind,lstore,A)
  end;
transform (<<"from">>,V,A) ->
  maps:put(from,naddress:decode(V),A);
transform (<<"to">>,V,A) ->
  maps:put(to,naddress:decode(V),A);
transform (<<"seq">>,V,A) when is_integer(V) ->
  maps:put(seq,V,A);
transform (<<"not_before">>,V,A) when is_integer(V) ->
  maps:put(not_before,V,A);
transform (<<"ver">>,2,A) ->
  maps:put(ver,2,A);
transform (<<"t">>,V,A) when is_integer(V) ->
  maps:put(t,V,A);
transform (<<"t">>,<<"now">>,A) ->
  maps:put(t,os:system_time(millisecond),A);
transform (<<"t">>,<<"NOW">>,A) ->
  maps:put(t,os:system_time(millisecond),A);
transform (<<"t">>,V,_A) ->
  throw({'cant_decode_timestamp',V});
transform (<<"txext">>,V,A) when is_map(V) ->
  maps:put(txext,
           maps:fold(
             fun(<<"code">>,Code,TA) ->
                 Read=fun(FFilename) ->
                          case file:read_file(FFilename) of
                            {ok, CBin} ->
                              CBin;
                            {error, Reason} ->
                              io:format("Can't read file ~s: ~p~n",
                                        [FFilename, Reason]),
                              throw('badfile')
                          end
                      end,

                 Code2=case Code of
                         <<"0x",Hex/binary>> ->
                           hex:decode(Hex);
                         <<"hex:",Hex/binary>> ->
                           hex:decode(Hex);
                         <<"b64:",Hex/binary>> ->
                           base64:decode(Hex);
                         <<"raw:",Hex/binary>> ->
                           Hex;
                         <<"hex@",CodeFilename/binary>> ->
                           hex:decode(Read(CodeFilename));
                         <<"b64@",CodeFilename/binary>> ->
                           base64:decode(Read(CodeFilename));
                         <<"raw@",CodeFilename/binary>> ->
                           Read(CodeFilename);
                         _ ->
                           base64:decode(Code)
                       end,
                 maps:put(code,Code2,TA);
                (<<"vm">>,VmName,TA) ->
                 maps:put(vm,binary_to_list(VmName),TA);
                (TK,TV,TA) ->
                 maps:put(TK,TV,TA)
             end, #{},V)
           ,A);

transform (<<"call">>,#{<<"function">>:=F,<<"args">>:=[<<"evmabi">>,<<"0x",TFunc/binary>>|Args]},A) ->
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
  Func=binary:decode_unsigned(hex:decode(TFunc)),
  maps:put(call,#{
                  function => binary_to_list(F),
                  args => [<<Func:32/big,BArgs/binary>>]
                 },A);


transform (<<"call">>,#{<<"function">>:=F,<<"args">>:=Arg},A) ->
  maps:put(call,#{
                  function=>binary_to_list(F),
                  args=>lists:map(
                          fun(<<"0x",Hex/binary>>) ->
                              hex:decode(Hex);
                             (Int) when is_integer(Int) ->
                              Int
                          end,Arg)
                 },A);

transform (<<"payload">>,V,A) when is_list(V) ->
  P=lists:map(
      fun([Purpose, Cur, Amount]) ->
          if is_integer(Amount) -> ok;
             true -> throw('bad_amount')
          end,
          #{amount=>Amount,
            cur=>(Cur),
            purpose=>if is_integer(Purpose) ->
                          tx:decode_purpose(Purpose);
                        Purpose==<<"transfer">> ->
                          transfer;
                        Purpose == <<"gas">> ->
                          gas;
                        Purpose == <<"dstfee">> ->
                          dstfee;
                        Purpose == <<"srcfee">> ->
                          srcfee
                     end
           }
      end, V),
  maps:put(payload,P,A);

transform (K,V,A) ->
  io:format("Unknown key ~s~n",[K]),
  maps:put(K,V,A).

