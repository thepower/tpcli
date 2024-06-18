-module(tpcli_main).

-export([main_run/2,run/2]).
-export([sign/1,submit/1,estimate/1,getsig/1]).
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
                 ({logs,_}) -> true;
                 ({revertdec,_}) -> true;
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
          io:format(standard_error,"\nError: ~p\n",[Atom]),
          exit(1)
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
           ok=file:write_file(Filename,
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
  io:format("set key ~p~n",[Filename]),
  L=case proplists:get_value(host,Opt,"httpsi://localhost:49800/") of
      "httpsi://localhost:49800/" ->
        Prev=readkey(Filename),
        case proplists:get_value(node,Prev) of
          undefined ->
            [];
          Host ->
            [{host,Host}]
        end;
      _ ->
        []
    end,
  io:format("set key ~p~n",[L]),
  R=L++[{keyfile,Filename}|proplists:delete(rawkey,proplists:delete(keyfile,Opt))],
  run(Rest, R);

run([{getcode,Addr}|Rest], Opt) ->
  {ok,R}=tpapi2:code(proplists:get_value(host,Opt),naddress:decode(Addr)),
  io:format("~s~n",[hex:encode(R)]),
  run(Rest, Opt);

run([{revertdec,Data}|Rest], Opt) ->
  R=eevm_abi_evsig:decode_auto(hex:decode(Data)),
  io:format("~p~n",[R]),
  run(Rest, Opt);

run([{logs,Height}|Rest], Opt) ->
  {ok,R}=tpapi2:get_log(proplists:get_value(host,Opt),list_to_integer(Height)),
  T=fun(List) ->
        lists:map(
          fun(Topic) when is_binary(Topic) ->
              hex:encodex(Topic);
             (Any) ->
              Any
          end,
          List)
    end,
  L=lists:foldr(
      fun([_TxID, <<"evm">>,<<"revert">>, <<>>],A) ->
          A;
         ([_TxID, <<"evm">>,<<"revert">>, <<Sig:4/binary,Data/binary>>],A) ->
          A;
          %[#{tx => TxID,
          %   revert=>true,
          %   from=>unknown,
          %   to=>unknown,
          %   sig => hex:encode(Sig),
          %   data => hex:encode(Data)}|A];
         ([TxID, <<"evm:revert">>, From, To, <<Sig:4/binary, Data/binary>>], A) ->
          [#{tx => TxID,
             revert=>true,
             from=>hex:encode(From),
             to=>hex:encode(To),
             sig => hex:encode(Sig),
             data => hex:encode(Data)}|A];
          ([TxID,<<"evm">>, From, To, Data, [Signature|Topics]],A) ->
          case getsig(Signature) of
            not_found ->
              [#{tx=>TxID,
                 from=>hex:encode(From),
                 to=>hex:encode(To),
                 data=>hex:encode(Data),
                 sig=>hex:encode(Signature),
                 topics=>T(Topics)
                }|A];
            {_, ABIIn, _} = ABI ->
              [#{tx=>TxID,
                 from=>hex:encode(From),
                 to=>hex:encode(To),
                 data=>hex:encode(Data),
                 decode => try
                             contract_evm_abi:decode_abi(Data, ABIIn, Topics,
                                                         fun(_,address,V) ->
                                                             hex:encodex(V);
                                                            (_,_,V) ->
                                                             V
                                                         end
                                                        )
                           catch Ec:Ee ->
                                   {error, {Ec,Ee}}
                           end,
                 sig=>contract_evm_abi:mk_sig(ABI),
                 topics=>T(Topics)
                }|A];
            [Found|_] = _All ->
              {ok,{_, ABI, _}}=contract_evm_abi:parse_signature(Found),
              [#{tx=>TxID,
                 from=>hex:encode(From),
                 to=>hex:encode(To),
                 data=>hex:encode(Data),
                 decode => try
                             contract_evm_abi:decode_abi(Data, ABI, Topics,
                                                         fun(_,address,V) ->
                                                             hex:encodex(V);
                                                            (_,_,V) ->
                                                             V
                                                         end
)
                           catch Ec:Ee ->
                                   {error, {Ec,Ee}}
                           end,
                 sig=>Found,
                 topics=>T(Topics)
                }|A]
          end
      end, [], R),
  io:format("~p~n",[L]),
  %lists:foreach(fun(Li) ->
  %                  io:format("~s~n",[jsx:encode(Li)])
  %              end,L),
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

run([sub|Rest], Opt) ->
  run(Rest, submit(Opt,[nowait]));

run([submit|Rest], Opt) ->
  run(Rest, submit(Opt,[]));

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

run([{construct,Filename}|Rest], Opt0) ->
  {ok,Bin} = file:read_file(Filename),
  JTX=jsx:decode(Bin,[return_maps]),
  JT=fun() ->
         case JTX of
            #{<<"from">>:=SA} ->
              case tp_construct:address(SA) of
                {ok, A, KeyFilename} ->
                  {[{address,naddress:encode(A)},{keyfile,KeyFilename}|Opt0],
                   JTX#{<<"from">>=>naddress:encode(A)}};
                {ok, A, undefined} ->
                  {Opt0, JTX};
                _Other ->
                  {Opt0,JTX}
              end;
            _ -> {Opt0,JTX}
          end
     end,
  {Opt,JTX1}=case lists:keyfind(keyfile,1,Opt0) of
               {keyfile,"tpcli.key"} ->
                 JT();
               false ->
                 JT();
               _ ->
                 {Opt0,JTX}
             end,
  JSON=case JTX1 of
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
                      {ok,S} -> S+1;
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
                  ok=file:write_file(RespFile,hex:encode(Bin));
                ".b64" ->
                  ok=file:write_file(RespFile,base64:encode(Bin));
                _ ->
                  ok=file:write_file(RespFile,Bin)
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
  ok=file:write_file("manifest.json",JSON),
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
  ok=file:write_file(
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
          case maps:get(from,Tx,undefined) of
            undefined -> ok;
            TxA ->
              MyA=naddress:decode(proplists:get_value(address, Prev)),
              if(TxA =/= MyA) ->
                  io:format("Registered address mismatch tx's from ~s =/= ~s~n",
                            [naddress:encode(MyA),naddress:encode(TxA)]);
                true ->
                  ok
              end
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
  submit(Opt, []).
submit(Opt, Extra) ->
  Tx=proplists:get_value(tx,Opt),
  if(Tx==undefined) ->
      throw('no_tx_for_submit');
    true ->
      ok
  end,
  io:format("tx to send ~s~n",[base64:encode(tx:pack(Tx))]),
  Res = tpapi2:submit_tx(
          proplists:get_value(host,Opt),
          tx:pack(Tx),
          Extra
         ),
  case Res of
    {ok, #{<<"ok">> := true,
           <<"block">> := Block,
           <<"txid">>:=TxID,
           <<"revert">> := Reason}} ->
      io:format("Tx submit revert~n TxID: ~s~n BlkID: ~s~n",[TxID, Block]),
      io:format(" Revert: ~p~n",[Reason]);
    {ok, #{<<"ok">> := true,
           <<"block">> := Block,
           <<"txid">>:=TxID,
           <<"retval">> := RetVal}} ->
      io:format("Tx submit ok~n TxID: ~s~n BlkID: ~s~n",[TxID, Block]),
      io:format(" Return: ~p~n",[RetVal]);
    {ok, #{<<"ok">> := true,<<"block">> := Block, <<"txid">>:=TxID}} ->
      io:format("Tx submit ok~n TxID: ~s~n BlkID: ~s~n",[TxID, Block]),
      ok;
    {ok, R} ->
      io:format("Tx submit ~p~n",[R]);
    {error, R} ->
      io:format("Tx submit error ~p~n",[R]),
      exit(1)
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
                  case tp_evm:estimate_gas(Node,ContractAddr,0,<<>>,#{code=>Code}) of
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
                  from:=FA,
                  to:=ContractAddr } ->
                  case tp_evm:estimate_gas(Node,ContractAddr,0,ABI,#{caller=>FA,origin=>FA}) of
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
                  from:=FA,
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

                  case tp_evm:estimate_gas(Node,ContractAddr,0,ABI,#{caller=>FA,origin=>FA}) of
                    {ok, _Res, Gas} ->
                      io:format("evm ret: ~p~n",[_Res]),
                      Gas;
                    _ ->
                      io:format("Estimate gas error~n"),
                      0
                  end;
                #{to:=ContractAddr, from:=FA } ->
                  ABI= <<>>,
                  case tp_evm:estimate_gas(Node,ContractAddr,0,ABI,#{caller=>FA,origin=>FA}) of
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

%%% TODO: port back from tpcli in tp_evsig !!!
getsig(<<221,242,82,173,27,226,200,155,105,194,176,104,252,55,141,170,149,43,167,241,
  99,196,161,22,40,245,90,77,245,35,179,239>>) ->
  {{function,<<"Transfer">>},
   [{<<"from">>,{indexed,address}},
    {<<"to">>,{indexed,address}},
    {<<"value">>,uint256}], undefined};

getsig(<<140,91,225,229,235,236,125,91,209,79,113,66,125,30,132,243,221,3,20,192,247,
  178,41,30,91,32,10,200,199,195,185,37>>) ->
  {{function,<<"Approval">>},
   [{<<"from">>,{indexed,address}},
    {<<"to">>,{indexed,address}},
    {<<"value">>,uint256}], undefined};

getsig(Any) ->
  AbiS=lists:foldl(
         fun(Filename,false) ->
             try
               ABI=contract_evm_abi:parse_abifile(Filename),
               R=contract_evm_abi:find_event_hash(Any,ABI),
               case R of
                 [] ->
                   false;
                 [Event] ->
                   Event
               end
             catch Ec:Ee:S ->
                     io:format("~p:~p @ ~p~n",[Ec,Ee,S]),
                     false
             end;
             (_,A) -> A
         end, false,
         filelib:wildcard("*.abi")
        ),
  if(AbiS == false) ->
      fetchsig(Any);
    true ->
      AbiS
  end.



%event Transfer(address indexed _from, address indexed _to, uint256 _value)
%event Approval(address indexed _owner, address indexed _spender, uint256 _value)

fetchsig(Sig) ->
  %curl https://www.4byte.directory/api/v1/event-signatures/\?hex_signature=0xBA6B0A89802623C9DB933568CE2F64B9D820F2243C46F0B10C4044E449AF3FC5 | jq '.results'
  io:format("~p~n",[Sig]),
  case tpapi2:httpget("https://www.4byte.directory",
                      list_to_binary(["/api/v1/event-signatures/?hex_signature=0x",binary:encode_hex(Sig)])) of
    #{<<"count">>:=0} ->
      not_found;
    #{<<"count">>:=_,<<"results">>:=R} ->
      io:format("~p~n",[R]),
      [ TS || #{<<"text_signature">>:=TS} <- R ]
  end.

