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
transform (<<"from">>,<<"0x",V/binary>>,A) ->
  maps:put(from,hex:decode(V),A);
transform (<<"from">>,V,A) ->
  maps:put(from,naddress:decode(V),A);
transform (<<"to">>,<<"0x",V/binary>>,A) ->
  maps:put(to,hex:decode(V),A);
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
transform (<<"patches">>, L, A) when is_list(L) ->
  maps:put(patches,
           lists:map(
             fun(#{<<"p">>:=Path,
                   <<"t">>:=Type,
                   <<"v">>:=Value}) ->
                 #{
                   <<"p">> => lists:map(
                                fun(<<"0x",Hex/binary>>) ->
                                    hex:decode(Hex);
                                   (Any) ->
                                    Any
                                end,
                                Path),
                   <<"t">> => Type,
                   <<"v">> => case Value of
                                <<"0x", Hex/binary>> ->
                                  hex:decode(Hex);
                                _ ->
                                  Value
                              end
                  }
             end, L), A);
transform (<<"txext">>,V,A) when is_map(V) ->
  maps:put(txext,
           maps:fold(
             fun(<<"code">>,Code,TA) ->
                 Code2=tp_readfile:parse_code(Code),
                 maps:put("code",Code2,TA);
                (<<"sponsor">>,Addresses,TA) when is_list(Addresses) ->
                 maps:put("sponsor",[naddress:decode(AA) || AA <- Addresses ],TA);
                (<<"vm">>,VmName,TA) ->
                 maps:put("vm",binary_to_list(VmName),TA);
                (TK,TV,TA) ->
                 maps:put(TK,TV,TA)
             end, #{},V)
           ,A);

transform (<<"call">>,<<"hex@stdin">>,_A) ->
      {ok, [X]} = io:fread("", "~s"),
      Data=hex:decode(string:strip(X)),
      #{<<"function">>=>"0x0",<<"args">>=>[Data]};

transform (<<"call">>,<<"json@stdin">>,A) ->
      {ok, [X]} = io:fread("", "~s"),
      case jsx:decode(list_to_binary(X),[return_maps]) of
        #{<<"function">>:=_,<<"args">>:=_}=M ->
          transform (<<"call">>,M,A);
        Other ->
        io:format("Invalid json for call: ~p~n",[Other]),
        exit(error)
      end;

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
                  args=>decode_json_args(Arg)
                 },A);

transform (<<"payload">>,V,A) when is_list(V) ->
  P=lists:map(
      fun([Purpose, Cur, Amount]) ->
          Am1=if is_integer(Amount) -> Amount;
                 is_float(Amount) -> trunc(Amount * 1.0e9);
             true -> throw('bad_amount')
          end,
          #{amount=>Am1,
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

decode_json_args(Args) ->
  lists:map(
    fun(<<"0x",B/binary>>) ->
        hex:decode(B);
       ([<<"!abiencode">>,Function2,Args2]) ->
        contract_evm_abi:encode_abi_call(decode_json_args(Args2),Function2);
       ([<<"!slice">>,Start,Len,Args2]) ->
        if(Len==0) ->
            <<_Skip:Start/binary,Use/binary>> = hd(decode_json_args([Args2])),
            Use;
          true ->
            <<_Skip:Start/binary,Use:Len/binary,_/binary>> = hd(decode_json_args([Args2])),
            Use
        end;
       ([<<"!sha3">>,Args2]) ->
        contract_evm_abi:keccak(
          hd(decode_json_args([Args2]))
         );
       (List) when is_list(List) ->
        decode_json_args(List);
       (Any) ->
        Any
    end, Args).
