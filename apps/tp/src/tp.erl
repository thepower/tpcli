-module(tp).

-export([main/1,get_config/0]).

main([]) ->
    getopt:usage(option_spec_list(), escript:script_name());
main(Args) ->
  config_logger(),
  OptSpecList = option_spec_list(),
  case getopt:parse(OptSpecList, Args) of
    {ok, {Options, NonOptArgs}} ->
      main_run(Options, NonOptArgs);
    {error, {Reason, Data}} ->
      io:format(standard_error,"Error: ~s ~p~n~n", [Reason, Data]),
      getopt:usage(OptSpecList, "tpcli")
  end.

main_run(Options, NonOpt) ->
  logger:info("Run as ~p~n",[Options]),
  Keys=lists:foldl(
         fun({keyfile,_},N) -> N+1;
            (_,N) -> N
         end,
         0,
         Options),
  {Act,Opt}=lists:partition(
              fun(A) when is_atom(A) -> true;
                 ({keyfile,_Filename}) when Keys>1 -> true;
                 ({construct,_Filename}) -> true;
                 ({evmcall,_Filename}) -> true;
                 ({savefilename,_Filename}) -> true;
                 ({address,_}) -> true;
                 ({getcode,_}) -> true;
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


option_spec_list() ->
  Conf=tp:get_config(),
  [
   %% {Name,   ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
   {host,       $h,        "host",       {string,
                                          proplists:get_value(host,Conf,"httpsi://localhost:49800/")},
    "tpnode's base address, use httpsi as protocol for ssl without cert verification"},
   {keyfile,    $k,        "keyfile",    {string,
                                          proplists:get_value(keyfile,Conf,"tpcli.key")},
    "keyfile"},
   {kgen,       undefined, "genkey",      undefined, "Export key"},
   {kexp,       undefined, "exportkey",   undefined, "Generate key"},
   {export_pw,  undefined, "exportpw",    string, "Password for export"},
   {ping,       undefined, "ping",        undefined, "ping node"},
   {sets,       undefined, "get_settings",undefined, "get chain settings"},
   {address,    undefined, "get_ledger", string, "get ledger for address"},
   {getcode,    undefined, "get_code", string, "get ledger's code for address"},
   {reg,        undefined, "register",    undefined, "run register"},
   {example,    undefined, "example",     undefined, "save example JSON for construct"},
   {construct,  undefined, "construct",   string, "Construct tx from JSON <filename>"},
   {sign,       undefined, "sign",        undefined, "Sign transaction"},
   {evmcall,    undefined, "evmcall",     string, "Call evm contract function, take args from <filename>"},
   {abifile,    undefined, "abi",     string, "Contract's ABI for result decoding"},
   {respfilename, undefined, "callresp",string, "Save evmcall resut to file <filename> "
    "(add .hex extension to save in hex format, .b64 in Base64, raw binary otherwise)"},
   {load,       undefined, "load",        string, "Load tx from <filename> in binary format"},
   {savefilename, undefined, "save",        string, "Save tx to <filename> in binary format"},
   {showtx,     undefined, "showtx",      undefined, "Display tx"},
   {submit,     undefined, "submit",      undefined, "Send transaction"},
   {{dry,true},     undefined, "dry",      undefined, "Do not actually sent transaction (for some operations)"},
   {mkmanifest, undefined, "mkmanifest", string, "Make manifest.json for specified directory"},
   {snewtask, undefined, "newstoragetask", undefined, "Create new storage task <address> <bucket name> <storage interval> <manifest file>"},
   {stask,    undefined, "get_task", undefined, "Get task info <address> <task id>"},
   {snewprov, undefined, "newprovider", undefined, "Create new storage provider <address> <uploadURL> <baseURL>"}
   %{token,     undefined, undefined,     string, "ceremony token"}
  ].

run([snewprov], Opt) ->
  case proplists:get_value(extra_arg,Opt) of
    [TAddress, UplURL, BaseURL] ->
      Addr=naddress:decode(TAddress),
      File=proplists:get_value(keyfile, Opt),
      Prev=readkey(File),
      MyAddr=case proplists:get_value(address, Prev) of
                undefined ->
                  throw('no_address');
                Adr ->
                  naddress:decode(Adr)
              end,
      Priv=case proplists:get_value(privkey, Prev) of
             undefined ->
               throw('no_key');
             TPriv ->
               hex:decode(TPriv)
           end,

      io:format("Addr ~p upload url ~s base url ~s~n",
                [Addr, UplURL, BaseURL]),

      case tp_storage:new_prov(
             proplists:get_value(host,Opt),
             Addr,
             MyAddr,
             Priv,
             UplURL,
             BaseURL,
             case proplists:get_value(dry,Opt) of
               true ->
                 [dry];
               _ ->
                 []
             end
            ) of
        {ok, R} ->
          io:format("Your provider ID is ~w~n",[R]);
        {error, Error} ->
          io:format("Error ~p~n",[Error])
      end;

    Any  ->
      io:format(standard_error, "Bad arguments: ~p~n",[Any])
  end,
  done;


run([snewtask], Opt) ->
  case proplists:get_value(extra_arg,Opt) of
    [TAddress,TBucket,TInterval,FManifest] ->
      Addr=naddress:decode(TAddress),
      Expire=parse_interval(TInterval)+os:system_time(second),
      {ok, ManifestBIN} = file:read_file(FManifest),
      #{hash:=Hash,
        size:=Size}=tp_manifest:parse_manifest(ManifestBIN),
      File=proplists:get_value(keyfile, Opt),
      Prev=readkey(File),
      MyAddr=case proplists:get_value(address, Prev) of
                undefined ->
                  throw('no_address');
                Adr ->
                  naddress:decode(Adr)
              end,
      Priv=case proplists:get_value(privkey, Prev) of
             undefined ->
               throw('no_key');
             TPriv ->
               hex:decode(TPriv)
           end,

      io:format("Addr ~p bucket ~p exp ~p manifest ~w~n~s",
                [Addr,TBucket, Expire, Size, hex:encode(Hash)]),

      case tp_storage:new_task(
             proplists:get_value(host,Opt),
             Addr,
             MyAddr,
             Priv,
             TBucket,
             Hash,
             Expire,
             Size,
             case proplists:get_value(dry,Opt) of
               true ->
                 [dry];
               _ ->
                 []
             end
            ) of
        {ok, R} ->
          io:format("Task ID: ~w~n~n",[R]),
          {ok,#{uploader:=UplID,
                name:=RBucket,
                owner:=RAddr}}=tp_storage:get_task(
                                 proplists:get_value(host,Opt),
                                 Addr,
                                 R,
                                 []),
          {ok,#{upload_url:=UURL,base_url:=BURL}}=tp_storage:get_prov(
                                                    proplists:get_value(host,Opt),
                                                    Addr,
                                                    UplID,
                                                    []),
          io:format("Your content will be served at ~s/~s/~s/~n",
                    [
                     BURL,
                     naddress:encode(binary:encode_unsigned(RAddr)),
                     RBucket
                    ]),
          io:format("Upload base address ~s~n",[UURL]),
          io:format("Example for manifest.json: ~s/~w/manifest.json~n",[UURL,R]),
          ok;

        {error, Error} ->
          io:format("Error ~p~n",[Error])
      end;

    Any  ->
      io:format(standard_error, "Bad arguments: ~p~n",[Any])
  end,
  done;

run([], _) ->
  done;

run([stask], Opt) ->
  case proplists:get_value(extra_arg,Opt) of
    [TAddress,TTaskID] ->
      Addr=naddress:decode(TAddress),
      TaskID=list_to_integer(TTaskID),
      {ok,#{uploader:=UplID,
            name:=RBucket,
            owner:=RAddr}=T}=tp_storage:get_task(
                               proplists:get_value(host,Opt),
                               Addr,
                               TaskID,
                               []),
      {ok,#{upload_url:=UURL,base_url:=BURL}}=tp_storage:get_prov(
                                                proplists:get_value(host,Opt),
                                                Addr,
                                                UplID,
                                                []),
      io:format("Owner: ~s~n",[naddress:encode(binary:encode_unsigned(RAddr))]),
      io:format("Status: ~w~n",[maps:get(status,T)]),
      io:format("Size: ~w~n",[maps:get(size,T)]),
      io:format("Created: ~s~n",[calendar:system_time_to_rfc3339(maps:get(time,T))]),
      io:format("Expire: ~s~n",[calendar:system_time_to_rfc3339(maps:get(expire,T))]),
      io:format("Hash: ~s~n",[hex:encode(binary:encode_unsigned(maps:get(hash,T)))]),
      io:format("Content will be served at ~s/~s/~s/~n",
                [
                 BURL,
                 naddress:encode(binary:encode_unsigned(RAddr)),
                 RBucket
                ]),
      io:format("Upload base address ~s~n",[UURL]),
      io:format("Example for manifest.json: ~s/~w/manifest.json~n",[UURL,TaskID]),
      ok;
    Any  ->
      io:format(standard_error, "Bad arguments: ~p~n",[Any])
  end,
  done;

run([snewtask|Rest], Opt) ->
  io:format(standard_error, "newstoragetask incompatible with other actions",[]),
  run(Rest, Opt);
run([snewprov|Rest], Opt) ->
  io:format(standard_error, "newprovider incompatible with other actions",[]),
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
      Keyfile=[{address,binary_to_list(TAddr)}|Prev],
      writefile(File, Keyfile),
      run(Rest, Opt)
  end;

run([example|Rest], Opt) ->
  Deploy="H4sIAMOhGWMCAyWOwQ6CMAyG7zzF0jMx22Jc4knOJnrw4IFwmK4iERgOYiCEd7fdemj7tX/+ds2EgB8GOAqdc/9pekcADofWLxBnr+A7nhWFkinUwRij9V7ppJh4fbneE434JVaxH+zSesuWJSFHCbUdIRdwO1PWUla0qJLNjDNbrVEKT++Qjd84n8bJB1vj7tH08Qi/HZ9CKsRbtmV/Sn7Kks0AAAA=",
  Tx="H4sIAAAAAAAAA22OMQ+CMBCFd35FczNDDyKim7OJDg4OxqGBgxChYIsGQ/jv9ihRBztc+r73kvfGQAh4koGtiEL+3yqdOwElaTJVBjMsTNsw3O1Q+pesUabpBjHxib7956frFS4+24fj2StLd6dRzqJTr7pVXHpxUogL9EZpW7hRoYDT3l28hotVKvuh0YdakxVEP3HHr753oIG7xzkJjS15SGfoSTk4Ns2pTNX1N1Q8dNZXreakHCT4FlCmtDySGcpIxhBiFHPVFEzBG7DjKIJJAQAA",
  EvmCall="H4sIAAAAAAAAA6vmUlBQSkxJKUotLlayUnJ0NDSAADNzQwMLCzNLC2MlHZCatNK85JLM/DwlKwWl9NSSgKL8ssyU1CKN0sy8EiNTM02IqsSidKAxCtFGsVy1XACpUKM9XAAAAA==",
  Save=fun(Bin,Filename) ->
           file:write_file(Filename,
                           zlib:gunzip(
                           base64:decode(Bin)
                            ))
       end,

  Save(Deploy, "example_deploy.json"),
  Save(Tx, "example_generic.json"),
  Save(EvmCall, "example_evmcall.json"),
  run(Rest, Opt);

run([{keyfile,Filename}|Rest], Opt) ->
  case filelib:is_regular(Filename) of
    true ->
      ok;
    false ->
      io:format("bad keyfile ~s~n",[Filename]),
      throw('bad_keyfile')
  end,
  run(Rest, [{keyfile,Filename}|proplists:delete(keyfile,Opt)]);

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
  Tx=proplists:get_value(tx,Opt),
  if(Tx==undefined) ->
      throw('no_tx_for_sign');
    true ->
      ok
  end,
  File=proplists:get_value(keyfile, Opt),
  Prev=readkey(File),
  case proplists:get_value(address, Prev) =/= undefined of
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
      run(Rest, [{tx,SignTx}|proplists:delete(tx,Opt)])
  end;

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

run([showtx|Rest], Opt) ->
  Tx=proplists:get_value(tx,Opt),
  if(Tx==undefined) ->
      throw('no_tx_for_show');
    true ->
      tp_show:tx(Tx)

  end,
  run(Rest, Opt);

run([submit|Rest], Opt) ->
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
    {ok, R} ->
      io:format("Tx submit ok ~p~n",[R]);
    {error, R} ->
      io:format("Tx submit error ~p~n",[R])
  end,
  run(Rest, Opt);

run([{construct,Filename}|Rest], Opt) ->
  {ok,Bin} = file:read_file(Filename),
  JTX=jsx:decode(Bin,[return_maps]),
  JSON=tp_construct:construct(JTX),
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

run([{evmcall,Filename}|Rest], Opt) ->
  {ok,FBin} = file:read_file(Filename),
  J=jsx:decode(FBin,[return_maps]),
  %io:format("Request ~p~n",[J]),
  case J of
    #{<<"address">> := TAddr,
      <<"args">> := Args,
      <<"function">> := Func} ->
      case tp_evm:q(
             proplists:get_value(host,Opt),
             naddress:decode(TAddr),
             Func,
             Args) of
        {return, Bin, _Stack} ->
          io:format("Contract finished successfully, returned: ~n~s~n",[tp_show:displaybin(Bin)]),
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
  Priv=tpecdsa:generate_priv(),
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

config_logger() ->
  logger:remove_handler(default),
  Config=#{
           config => #{ type=>standard_error },
           level => info,
           filters => [{nosasl, {fun logger_filters:progress/2, stop}}]
          },
  logger:add_handler(default, logger_std_h, Config).

parse_interval(Str) ->
  parse_interval1(string:tokens(Str," "),0).

parse_interval1([],Int) -> Int;

parse_interval1([N,"year"++_|Rest],Int) ->
  parse_interval1(Rest,
                  Int+(list_to_integer(N)*86400*365)
                 );
parse_interval1([N,"mon"++_|Rest],Int) ->
  parse_interval1(Rest,
                  Int+(list_to_integer(N)*86400*30)
                 );
parse_interval1([N,"week"++_|Rest],Int) ->
  parse_interval1(Rest,
                  Int+(list_to_integer(N)*86400*7)
                 );
parse_interval1([N,"day"++_|Rest],Int) ->
  parse_interval1(Rest,
                  Int+list_to_integer(N)*86400
                 );
parse_interval1([N,"hour"++_|Rest],Int) ->
  parse_interval1(Rest,
                  Int+list_to_integer(N)*3600
                 );
parse_interval1([N,"min"++_|Rest],Int) ->
  parse_interval1(Rest,
                  Int+list_to_integer(N)*60
                 );
parse_interval1([N,"sec"++_|Rest],Int) ->
  parse_interval1(Rest,
                  Int+list_to_integer(N)
                 ).


get_config() ->
  {ok, [[Home]]} = init:get_argument(home),
  ConfDir = filename:join(Home, ".config/tp"),
  Filename = filename:join(ConfDir, "cli.config"),
  case file:consult(Filename) of
    {ok, Config} ->
      Config;
    _ ->
      []
  end.
