-module(tp).

-export([main/1,get_config/0]).

main([]) ->
    getopt:usage(options_main(), escript:script_name());

main(["storage"]) ->
  getopt:usage(options_storage(), escript:script_name());

main(["info"]) ->
  getopt:usage(options_info(), escript:script_name());

main(["showabi",Filename]) ->
  File=contract_evm_abi:parse_abifile(Filename),
  if(File==[]) ->
      io:format("~s is empty~n",[Filename]);
    true ->
      lists:foreach(
        fun({{Kind,_Name},_In,_Out}=Signature) ->
            io:format("~p ~s #~s~n",
                      [Kind,
                       contract_evm_abi:mk_fullsig(Signature),
                       if Kind==constructor -> <<>>;
                          true ->
                            hex:encode(contract_evm_abi:sigb32(contract_evm_abi:mk_sig(Signature)))
                       end
                      ])
        end,
        File)
  end,
  ok;

main(["tpas"]) ->
  config_logger(),
  io:format("~p~n",[application:ensure_all_started(tpas)]),
  inet:i(),
  io:format("Running tpas, hit ^C to terminate"),
  receive Any -> Any end;

main(["server"]) ->
  config_logger(),
  io:format("~p~n",[application:ensure_all_started(ranch)]),
  tp_http_handler:start(),
  inet:i(),
  io:format("Running tp server, hit ^C to terminate"),
  receive Any -> Any end;

main(["info"|Args]) ->
  config_logger(),
  OptSpecList = options_info(),
  case getopt:parse(OptSpecList, Args) of
    {ok, {Options, NonOptArgs}} ->
      tpcli_info:main_run(Options, NonOptArgs);
    {error, {Reason, Data}} ->
      io:format(standard_error,"Error: ~s ~p~n~n", [Reason, Data]),
      getopt:usage(OptSpecList, "tpcli")
  end;


main(["storage"|Args]) ->
  config_logger(),
  OptSpecList = options_storage(),
  case getopt:parse(OptSpecList, Args) of
    {ok, {Options, NonOptArgs}} ->
      tpcli_storage:main_run(Options, NonOptArgs);
    {error, {Reason, Data}} ->
      io:format(standard_error,"Error: ~s ~p~n~n", [Reason, Data]),
      getopt:usage(OptSpecList, "tpcli")
  end;

main(Args) ->
  config_logger(),
  OptSpecList = options_main(),
  case getopt:parse(OptSpecList, Args) of
    {ok, {Options, NonOptArgs}} ->
      tpcli_main:main_run(Options, NonOptArgs);
    {error, {Reason, Data}} ->
      io:format(standard_error,"Error: ~s ~p~n~n", [Reason, Data]),
      getopt:usage(OptSpecList, "tpcli")
  end.

options_info() ->
  Conf=tp:get_config(),
  [
   %% {Name,   ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
   {host,       $h,        "host",       {string,
                                          proplists:get_value(host,Conf,"httpsi://localhost:49800/")},
    "tpnode's base address, use httpsi as protocol for ssl without cert verification"},
   {chain,      $c,       "chain",      {integer, 0}, "chain number, 0 - node's chain"},
   {fetch_genesis, undefined, "fetch_genesis", undefined, "Fetch node's chain genesis block"},
   {nodes, undefined, "nodes", undefined, "get chain's nodes"},
   {candidates, undefined, "candidates", undefined, "analyze block candidates"}
  ].



options_main() ->
  Conf=tp:get_config(),
  [
   %% {Name,   ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
   {host,       $h,        "host",       {string,
                                          proplists:get_value(host,Conf,"httpsi://localhost:49800/")},
    "tpnode's base address, use httpsi as protocol for ssl without cert verification"},
   {rawkey,    undefined,  "hexkey",    string, "keyfile"},
   {keyfile,    $k,        "keyfile",    undefined,
    "keyfile"},
   {{ed25519,true}, undefined, "ed25519", undefined, "use ed25519 for keys generation"},
   {kgen,       undefined, "genkey",      undefined, "Generate key"},
   {kexp,       undefined, "exportkey",   undefined, "Export key"},
   {wexp,       undefined, "exportwif",   undefined, "Export key as wif"},
   {wimp,       undefined, "importwif",   string, "Import key from wif"},
   {export_pw,  undefined, "exportpw",    string, "Password for export"},
   {ping,       undefined, "ping",        undefined, "ping node"},
   {sets,       undefined, "get_settings",undefined, "get chain settings"},
   {address,    undefined, "get_ledger", string, "get ledger for address"},
   {getcode,    undefined, "get_code", string, "get ledger's code for address"},
   {logs,    undefined, "logs", string, "get logs for block with height"},
   {reg,        undefined, "register",    undefined, "run register"},
   {example,    undefined, "example",     undefined, "save example JSON for construct"},
   {construct,  undefined, "construct",   string, "Construct tx from JSON <filename>"},
   {evmcall,    undefined, "evmcall",     string, "Call evm contract function, take args from <filename>"},
   {revertdec, undefined, "revertdec", string, "Decode revert <hex string>"},
   {abifile,    undefined, "abi",     string, "Contract's ABI for result decoding"},
   {respfilename, undefined, "callresp",string, "Save evmcall resut to file <filename> "
    "(add .hex extension to save in hex format, .b64 in Base64, raw binary otherwise)"},
   {load,       undefined, "load",        string, "Load tx from <filename> in binary format"},
   {gasprice, undefined, "gasprice",      integer, "Calc gas price"},
   {estimate, undefined, "estimate",      undefined, "Estimate fee and gas"},
   {savefilename, undefined, "save",      string, "Save tx to <filename> in binary format"},
   {sign,       undefined, "sign",        undefined, "Sign transaction"},
   {showtx,     undefined, "showtx",      undefined, "Display tx"},
   {submit,     undefined, "submit",      undefined, "Send transaction"},
   {sub,     undefined, "sub",      undefined, "Like submit, but without waiting"},
   {ss,   undefined, "ss",  undefined, "Sign & submit"}
  ].


options_storage() ->
  Conf=tp:get_config(),
  [
   %% {Name,   ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
   {host,      $h,        "host",       {string,
                                          proplists:get_value(host,Conf,"httpsi://localhost:49800/")},
    "tpnode's base address, use httpsi as protocol for ssl without cert verification"},
   {rawkey,    undefined,  "hexkey",    string, "hex file"},
   {keyfile,   $k,        "keyfile",    {string,
                                          proplists:get_value(keyfile,Conf,"tpcli.key")},
    "keyfile"},
   {mkmanifest, undefined, "mkmanifest", string, "Make manifest.json for specified directory"},
   {bucketname,  undefined, "newstoragetask", string, "Create new storage task <bucket name>"},
   {manifest,  undefined, "manifest", {string, "manifest.json"}, "manifest filename"},
   {interval,  undefined, "interval", {string, "1 year"}, "Store interval"},
   {transfer,  undefined, "transfer", string, "Transfer amount and currency, ex: \"10 SK\""},
   {gas,       undefined, "gas", string, "Transfer amount and currency, ex: \"10 SK\""},
   %"<storage interval> <manifest file>"},
   {stask,     undefined, "get_task", integer, "Get task info <task id>"},
   {baseurl,   undefined, "newprovider", string, "Create new storage provider with <baseurl> or <baseurl,uploadurl> "},
   {sign,      undefined, "sign",    undefined, "Sign transaction"},
   {showtx,    undefined, "showtx",  undefined, "Display tx"},
   {estimate, undefined, "estimate",      undefined, "Estimate fee and gas"},
   {submit,    undefined, "submit",  undefined, "Send transaction"},
   {ss,        undefined, "ss",  undefined, "Sign & submit"},
   {address,   undefined, undefined, string, "Storage contract address"}

   %{token,     undefined, undefined,     string, "ceremony token"}
  ].

config_logger() ->
  logger:remove_handler(default),
  Config=#{
           config => #{ type=>standard_error },
           level => info,
           filters => [{nosasl, {fun logger_filters:progress/2, stop}}]
          },
  logger:add_handler(default, logger_std_h, Config).

get_config(Filename) ->
  case file:consult(Filename) of
    {ok, Config} ->
      Config;
    _ ->
      []
  end.

get_config() ->
  {ok, [[Home]]} = init:get_argument(home),
  ConfDir = filename:join(Home, ".config/tp"),
  Filename = filename:join(ConfDir, "cli.config"),
  get_config(".tpcli.config")++
  get_config(Filename).

