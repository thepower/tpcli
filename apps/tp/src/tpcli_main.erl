-module(tpcli_main).

-export([main_run/2,run/2]).
-export([sign/1,submit/1,estimate/1]).
-include_lib("public_key/include/public_key.hrl").

main_run(Options, NonOpt) ->
  logger:info("Run as ~p~n",[Options]),
  Keys=lists:foldl(
         fun({keyfile,_},N) -> N+1;
            ({rawkey,_},N) -> N+1;
            (_,N) -> N
         end,
         0,
         Options),
  {Act,Opt}=lists:partition(
              fun(A) when is_atom(A) -> true;
                 ({keyfile,_Filename}) when Keys>1 -> true;
                 ({rawkey,_}) when Keys>1 -> true;
                 ({construct,_Filename}) -> true;
                 ({evmcall,_Filename}) -> true;
                 ({savefilename,_Filename}) -> true;
                 ({address,_}) -> true;
                 ({wimp,_}) -> true;
                 ({getcode,_}) -> true;
                 ({gasprice,_}) -> true;
                 ({mkmanifest,_}) -> true;
                 ({load,_Filename}) -> true;
                 (_) -> false
              end,Options),
  try
    application:ensure_all_started(tp),
    logger:info("Run ~p~n",[Act]),
    Opts=[{extra_arg,NonOpt}|Opt],
    logger:info("Opts ~p~n",[Opts]),
    run(Act, Opts)
  catch throw:Atom ->
          io:format(standard_error,"\nError: ~p\n",[Atom])
  end.

run([], _) ->
  done;

run([{gasprice, N}|Rest], Opt) ->
  Gas=tpapi2:gas_price(N,proplists:get_value(host,Opt)),
  io:format("~p~n",[Gas]),
  run(Rest, Opt);


run([reg|Rest], Opt) ->
  File=proplists:get_value(keyfile, Opt),
  Prev=readkey(File),
  case proplists:get_value(address, Prev) =/= undefined of
    true ->
      io:format("Address already registered ~s~n",[proplists:get_value(address, Prev)]),
      throw('address_exists');
    false ->
      ok
  end,
  case proplists:get_value(privkey, Prev) of
    undefined ->
      throw('no_key');
    TPriv ->
      Priv=hex:decode(TPriv),
      {ok, #{<<"address">>:=TAddr}} = tpapi2:reg(
                                        proplists:get_value(host,Opt),
                                        Priv
                                       ),
      io:format("Registered address is ~s~n",[TAddr]),
      Keyfile=[{address,binary_to_list(TAddr)},
               {node,proplists:get_value(host,Opt)}|Prev],
      writefile(File, Keyfile),
      run(Rest, Opt)
  end;

run([example|Rest], Opt) ->
  Deploy="H4sIAMOhGWMCAyWOwQ6CMAyG7zzF0jMx22Jc4knOJnrw4IFwmK4iERgOYiCEd7fdemj7tX/+ds2EgB8GOAqdc/9pekcADofWLxBnr+A7nhWFkinUwRij9V7ppJh4fbneE434JVaxH+zSesuWJSFHCbUdIRdwO1PWUla0qJLNjDNbrVEKT++Qjd84n8bJB1vj7tH08Qi/HZ9CKsRbtmV/Sn7Kks0AAAA=",
  Tx="H4sIAAAAAAAAA22OMQ+CMBCFd35FczNDDyKim7OJDg4OxqGBgxChYIsGQ/jv9ihRBztc+r73kvfGQAh4koGtiEL+3yqdOwElaTJVBjMsTNsw3O1Q+pesUabpBjHxib7956frFS4+24fj2StLd6dRzqJTr7pVXHpxUogL9EZpW7hRoYDT3l28hotVKvuh0YdakxVEP3HHr753oIG7xzkJjS15SGfoSTk4Ns2pTNX1N1Q8dNZXreakHCT4FlCmtDySGcpIxhBiFHPVFEzBG7DjKIJJAQAA",
  EvmCall="H4sIAAAAAAAAA6vmUlBQSkxJKUotLlayUnJ0NDSAADNzQwMLCzNLC2MlHZCatNK85JLM/DwlKwWl9NSSgKL8ssyU1CKN0sy8EiNTM02IqsSidKAxCtFGsVy1XACpUKM9XAAAAA==",
  LStore="H4sIAAAAAAAAA63PQQ+CIBwF8Lufgv3PHACbWTdvnerYoblmQdOFYsCs5vzugZSnNi9yeu/Bfht9hBB0QsMWMezzvWq4KyCNVVrAuN20qv2WZZSEk6wpSdNkk8bhhfXX+8MxNCMek9cWb6kKT57y72CvpTB+cBWhHlqfQYpOSAo4BOZCKaRUkOORd6gFFzsXd/4Co6fSksOAZ5hL1VAWr/5B5EUoYSSeR2Rl7CT4ci44/zGVFTVdwGALGO4vjsijIfoA/W50ENsBAAA=",
  Save=fun(Bin,Filename) ->
           file:write_file(Filename,
                           zlib:gunzip(
                           base64:decode(Bin)
                            ))
       end,

  Save(Deploy, "example_deploy.json"),
  Save(Tx, "example_generic.json"),
  Save(LStore, "example_lstore.json"),
  Save(EvmCall, "example_evmcall.json"),
  run(Rest, Opt);

run([{rawkey,Hex}|Rest], Opt) ->
  run(Rest, [{rawkey,Hex}|proplists:delete(rawkey,Opt)]);

run([{keyfile,Filename}|Rest], Opt) ->
  case filelib:is_regular(Filename) of
    true ->
      ok;
    false ->
      io:format("bad keyfile ~s~n",[Filename]),
      throw('bad_keyfile')
  end,
  run(Rest, [{keyfile,Filename}|proplists:delete(rawkey,proplists:delete(keyfile,Opt))]);

run([{getcode,Addr}|Rest], Opt) ->
  {ok,R}=tpapi2:code(proplists:get_value(host,Opt),naddress:decode(Addr)),
  io:format("~s~n",[hex:encode(R)]),
  run(Rest, Opt);

run([{address,Addr}|Rest], Opt) ->
  {ok,R}=tpapi2:ledger(proplists:get_value(host,Opt),naddress:decode(Addr)),
  EHF=fun([{Type, Str}|Tokens],{parser, State, Handler, Stack}, Conf) ->
          Conf1=jsx_config:list_to_config(Conf),
          Hex=hex:encode(Str),
          jsx_parser:resume([{Type, <<"0x",Hex/binary>>}|Tokens],
                            State, Handler, Stack, Conf1)
      end,
  io:format("~s~n",[jsx:prettify(jsx:encode(R,[strict, {error_handler, EHF}]))]),
  run(Rest, Opt);

run([sets|Rest], Opt) ->
  {ok,R}=tpapi2:settings(proplists:get_value(host,Opt)),
  EHF=fun([{Type, Str}|Tokens],{parser, State, Handler, Stack}, Conf) ->
          Conf1=jsx_config:list_to_config(Conf),
          Hex=hex:encode(Str),
          jsx_parser:resume([{Type, <<"0x",Hex/binary>>}|Tokens],
                            State, Handler, Stack, Conf1)
      end,
  io:format("~s~n",[jsx:prettify(jsx:encode(R,[strict, {error_handler, EHF}]))]),
  run(Rest, Opt);

run([sign|Rest], Opt) ->
  run(Rest, sign(Opt));

run([submit|Rest], Opt) ->
  run(Rest, submit(Opt));

run([ss|Rest], Opt) ->
  run(Rest, tpcli_main:submit(tpcli_main:sign(Opt)));

run([{load,Filename}|Rest], Opt) ->
  {ok, Bin} = file:read_file(Filename),
  Tx=tx:unpack(Bin),
  run(Rest, [{tx,Tx}|Opt]);

run([{savefilename,Filename}|Rest], Opt) ->
  Tx=proplists:get_value(tx,Opt),
  if(Tx==undefined) ->
      throw('no_tx_for_save');
    true ->
      ok
  end,
  ok=file:write_file(Filename, tx:pack(Tx)),
  io:format("Tx written to file ~s~n",[Filename]),
  run(Rest, Opt);

run([estimate|Rest], Opt) ->
  estimate(Opt),
  run(Rest, Opt);


run([showtx|Rest], Opt) ->
  Tx=proplists:get_value(tx,Opt),
  if(Tx==undefined) ->
      throw('no_tx_for_show');
    true ->
      tp_show:tx(Tx)

  end,
  run(Rest, Opt);

run([{construct,Filename}|Rest], Opt) ->
  {ok,Bin} = file:read_file(Filename),
  JTX=jsx:decode(Bin,[return_maps]),
  JSON=case JTX of
         #{ <<"seq">>:= <<"auto">> } ->
           Node=proplists:get_value(host,Opt),
           File=proplists:get_value(keyfile, Opt),
           Prev=readkey(File),
           Seq  = case proplists:get_value(address, Prev) of
                  undefined ->
                    #{};
                  Adr ->
                    MyAddr=naddress:decode(Adr),
                    case tpapi2:get_seq(Node, MyAddr) of
                      {ok,S} -> S;
                      Err ->
                        logger:notice("Can't get ledger for address ~s: ~p",[naddress:encode(MyAddr), Err]),
                        0
                    end
                end,

           io:format("Autoseq ~w~n",[Seq]),
           tp_construct:construct(JTX#{<<"seq">>=>Seq});
         _ ->
           tp_construct:construct(JTX)
       end,
  Tx=tx:construct_tx(JSON),

  run(Rest, [{tx,Tx}|Opt]);


run([kexp|Rest], Opt) ->
  File=proplists:get_value(keyfile, Opt),
  Prev=readkey(File),
  case proplists:get_value(privkey, Prev) of
    undefined ->
      throw('no_key');
    Value ->
      PEM=case proplists:get_value(export_pw, Opt) of
        undefined ->
          tpecdsa:export(hex:decode(Value),pem);
        Pw ->
          tpecdsa:export(hex:decode(Value),pem,Pw)
      end,
      io:format("~s~n",[PEM]),
      run(Rest, Opt)
  end;

run([{wimp,WIF}|Rest], Opt) ->
  File=proplists:get_value(keyfile, Opt),
  Prev=readkey(File),
  case proplists:get_value(privkey, Prev) of
    undefined ->
      ok;
    _ ->
      throw('key_exists')
  end,
  Priv=wif:parsekey(list_to_binary([WIF])),
  HexPriv=binary_to_list(hex:encode(Priv)),
  HexPub=binary_to_list(hex:encode(tpecdsa:calc_pub(Priv,true))),
  Keyfile=[{privkey,HexPriv},
           {pubkey,HexPub}
           |Prev],
  writefile(File, Keyfile),
  run(Rest,Opt);


run([wexp|Rest], Opt) ->
  File=proplists:get_value(keyfile, Opt),
  Prev=readkey(File),
  case proplists:get_value(privkey, Prev) of
    undefined ->
      throw('no_key');
    HValue ->
      Value=hex:decode(HValue),
      case tpecdsa:keytype(Value) of
        {priv, secp256k1_legacy} ->
          io:format("~s~n",[wif:encodekey(Value)]),
          run(Rest, Opt);
        {priv, secp256k1} ->
          #'ECPrivateKey'{
             version = 1,
             privateKey = PrivKey,
             parameters = {namedCurve,_Curve} } = public_key:der_decode('PrivateKeyInfo', Value),
          io:format("~s~n",[wif:encodekey(PrivKey)]),
          run(Rest, Opt);
        _Other ->
          io:format("Used key's type (~p) does not supported for WIF export", [_Other])
      end
  end;

run([{evmcall,Filename}|Rest], Opt) ->
  {ok,FBin} = file:read_file(Filename),
  J=jsx:decode(FBin,[return_maps]),
  %io:format("Request ~p~n",[J]),
  case J of
    #{<<"address">> := TAddr,
      <<"args">> := Args,
      <<"function">> := Func} ->
      case tp_evm:q_raw(
             proplists:get_value(host,Opt),
             naddress:decode(TAddr),
             Func,
             Args) of
        {done, {return,Bin}, #{gas:=GasLeft}} ->
          io:format("Contract finished successfully, returned: ~n~s~n",[tp_show:displaybin(Bin)]),
          io:format("Gas burned ~p~n",[1000000000-GasLeft]),
          case proplists:get_value(abifile, Opt) of
            undefined ->
              ok;
            ABIFilename ->
              ABI = tp_abi:parse_abifile(ABIFilename),
              case tp_abi:find_function(ABI, binary_to_list(Func)) of
                {_Fun,_In,Outs} ->
                  Decode=tp_abi:decode_abi(Bin, Outs),
                  io:format("Outs ~p~n",[Decode]);
                _ ->
                  ignore
              end
          end,
          case proplists:get_value(respfilename, Opt) of
            undefined -> ok;
            RespFile ->
              case filename:extension(RespFile) of
                ".hex" ->
                  file:write_file(RespFile,hex:encode(Bin));
                ".b64" ->
                  file:write_file(RespFile,base64:encode(Bin));
                _ ->
                  file:write_file(RespFile,Bin)
              end
          end;
        Other ->
          io:format("Contract returned ~p~n",[Other])
      end
  end,
  run(Rest, Opt);

run([kgen|Rest], Opt) ->
  File=proplists:get_value(keyfile, Opt),
  Prev=readkey(File),
  case proplists:get_value(privkey, Prev) of
    undefined ->
      ok;
    _ ->
      throw('key_exists')
  end,
  Priv=case proplists:get_value(ed25519,Opt, false) of
         false -> tpecdsa:generate_priv();
         true -> tpecdsa:generate_priv(ed25519)
       end,
  HexPriv=binary_to_list(hex:encode(Priv)),
  HexPub=binary_to_list(hex:encode(tpecdsa:calc_pub(Priv,true))),
  Keyfile=[{privkey,HexPriv},
           {pubkey,HexPub}
           |Prev],
  writefile(File, Keyfile),
  run(Rest,Opt);

run([ping|Rest], Opt) ->
  R=try
      tpapi2:ping(proplists:get_value(host,Opt))
    catch _:_ ->
            false
    end,
  io:format("Ping ~p~n",[R]),
  run(Rest,Opt);

run([{mkmanifest,Dirname}|Rest], Opt) ->
  {ok,#{manifest:=JSON, size:=Size, files_count:=C, hash:= Hash}} = tp_manifest:dir2manifest(Dirname),
  io:format("Files found: ~w~n",[C]),
  io:format("Manifest writtent to manifest.json~n"),
  io:format("Manifest hash: ~s~n",[hex:encode(Hash)]),
  io:format("Total files size: ~w~n",[Size]),
  file:write_file("manifest.json",JSON),
  run(Rest,Opt);

run([Other|Rest], Opt) ->
  logger:error("I don't know how to do ~p~n",[Other]),
  run(Rest, Opt).

readkey(KeyFile) ->
  case file:consult(KeyFile) of
    {ok, Data} ->
      Data;
    _ ->
      []
  end.

writefile(Filename, Proplist) ->
  file:write_file(
    Filename,
  [
   io_lib:format("~p.~n",[E])
   ||
   E <- Proplist
  ]).

sign(Opt) ->
  Tx=proplists:get_value(tx,Opt),
  if(Tx==undefined) ->
      throw('no_tx_for_sign');
    true ->
      ok
  end,

  case proplists:get_value(rawkey, Opt) of
    undefined ->
      File=proplists:get_value(keyfile, Opt),
      Prev=readkey(File),
      case proplists:get_value(address, Prev) =/= undefined of
        false ->
          ok;
        true ->
          TxA=maps:get(from,Tx),
          MyA=naddress:decode(proplists:get_value(address, Prev)),
          if(TxA =/= MyA) ->
              io:format("Registered address mismatch tx's from ~s =/= ~s~n",
                        [naddress:encode(MyA),naddress:encode(TxA)]);
            true ->
              ok
          end
      end,
      case proplists:get_value(privkey, Prev) of
        undefined ->
          throw('no_key');
        TPriv ->
          Priv=hex:decode(TPriv),
          SignTx=tx:sign(Tx,Priv),
          [{tx,SignTx}|proplists:delete(tx,Opt)]
      end;
    HexKey ->
      Priv=hex:decode(HexKey),
      SignTx=tx:sign(Tx,Priv),
      [{tx,SignTx}|proplists:delete(tx,Opt)]
  end.

submit(Opt) ->
  Tx=proplists:get_value(tx,Opt),
  if(Tx==undefined) ->
      throw('no_tx_for_submit');
    true ->
      ok
  end,
  Res = tpapi2:submit_tx(
          proplists:get_value(host,Opt),
          tx:pack(Tx)
         ),
  case Res of
    {ok, #{<<"ok">> := true,<<"block">> := Block, <<"txid">>:=TxID, <<"retval">> := RetVal}} ->
      io:format("Tx submit ok~n TxID: ~s~n BlkID: ~s~n",[TxID, Block]),
      io:format(" Return: ~p~n",[RetVal]);
    {ok, #{<<"ok">> := true,<<"block">> := Block, <<"txid">>:=TxID}} ->
      io:format("Tx submit ok~n TxID: ~s~n BlkID: ~s~n",[TxID, Block]),
      ok;
    {ok, R} ->
      io:format("Tx submit ~p~n",[R]);
    {error, R} ->
      io:format("Tx submit error ~p~n",[R])
  end,
  lists:foreach(
    fun({submitcb, F}) when is_function(F) ->
        try
          F(Res)
        catch _:_ -> ok 
        end;
       (_) ->
        ok
    end, Opt),
  Opt.

estimate(Opt) ->
  Tx=proplists:get_value(tx,Opt),
  if(Tx==undefined) ->
      throw('no_tx_for_estimate');
    true ->
      Node=proplists:get_value(host,Opt),
      GasNeed=case Tx of
                #{kind:=deploy,
                  txext := #{"code" := Code,"vm":="evm"},
                  from:=ContractAddr } ->
                  case tp_evm:estimate_gas(Node,ContractAddr,0,<<>>,Code) of
                    {ok, {return,_Res}, Gas} ->
                      io:format("evm returned ~p bytes~n",[size(_Res)]),
                      Gas;
                    {ok, _Res, Gas} ->
                      io:format("evm ret: ~p~n",[_Res]),
                      Gas;
                    Any ->
                      io:format("Estimate gas error: ~p~n",[Any]),
                      0
                  end;

                #{call:=#{
                          function := "0x0",
                          args := [ABI]
                         },
                  to:=ContractAddr } ->
                  case tp_evm:estimate_gas(Node,ContractAddr,0,ABI) of
                    {ok, _Res, Gas} ->
                      io:format("evm ret: ~p~n",[_Res]),
                      Gas;
                    Any ->
                      io:format("Estimate gas error: ~p~n",[Any]),
                      0
                  end;

                #{call:=#{
                          function := SFunc,
                          args := Args
                         },
                  to:=ContractAddr } ->
                  Func=list_to_binary(SFunc),
                  IFun = fun(<<"0x",Hex:8/binary>>) ->
                             B=hex:decode(Hex),
                             binary:decode_unsigned(B);
                            (B) when is_binary(B) ->
                             <<X:32/big,_/binary>> = esha3:keccak_256(B),
                             X
                         end(Func),
                  BArgs=tpapi2:evm_encode(
                          lists:map(
                            fun(<<"0x",Hex/binary>>) ->
                                {bin, hex:decode(Hex)};
                               (Int) when is_integer(Int) ->
                                Int;
                               (Other) when is_list(Other) ->
                                list_to_binary(Other);
                               (Other) when is_binary(Other) ->
                                Other
                            end,Args)
                         ),
                  ABI  = << IFun:32/big, BArgs/binary>>,

                  case tp_evm:estimate_gas(Node,ContractAddr,0,ABI) of
                    {ok, _Res, Gas} ->
                      io:format("evm ret: ~p~n",[_Res]),
                      Gas;
                    _ ->
                      io:format("Estimate gas error~n"),
                      0
                  end;
                #{to:=ContractAddr } ->
                  ABI= <<>>,
                  case tp_evm:estimate_gas(Node,ContractAddr,0,ABI) of
                    {ok, _Res, Gas} ->
                      io:format("evm ret: ~p~n",[_Res]),
                      Gas;
                    _ ->
                      io:format("Estimate gas error~n"),
                      0
                  end;
                _ ->
                  0
              end,
      io:format("Gas need ~w~n",[GasNeed]),
      GasPrice=tpapi2:gas_price(GasNeed,Node),
      io:format("GasEstimate: ~p~n",[GasPrice]),
      io:format("GasEstimate/10^9: ~p~n",[maps:map(fun(_,V) -> V/1.0e9 end, GasPrice)]),
      TxSize=size(maps:get(body,Tx)),
      FeePrice=tpapi2:fee_price(TxSize,Node),
      io:format("Tx size ~w~n",[TxSize]),
      io:format("FeeEstimate: ~p~n",[FeePrice]),
      io:format("FeeEstimate/10^9: ~p~n",[maps:map(fun(_,V) -> V/1.0e9 end, FeePrice)]),
      Opt
  end.

