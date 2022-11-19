-module(tp).

-export([main/1,get_config/0]).

main([]) ->
    getopt:usage(options_main(), escript:script_name());

main(["storage"]) ->
  getopt:usage(options_storage(), escript:script_name());

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

options_main() ->
  Conf=tp:get_config(),
  [
   %% {Name,   ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
   {host,       $h,        "host",       {string,
                                          proplists:get_value(host,Conf,"httpsi://localhost:49800/")},
    "tpnode's base address, use httpsi as protocol for ssl without cert verification"},
   {rawkey,    undefined,  "hexkey",    string, "keyfile"},
   {keyfile,    $k,        "keyfile",    {string,
                                          proplists:get_value(keyfile,Conf,"tpcli.key")},
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
   {reg,        undefined, "register",    undefined, "run register"},
   {example,    undefined, "example",     undefined, "save example JSON for construct"},
   {construct,  undefined, "construct",   string, "Construct tx from JSON <filename>"},
   {evmcall,    undefined, "evmcall",     string, "Call evm contract function, take args from <filename>"},
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
   {ss,   undefined, "ss",  undefined, "Sign & submit"}
  ].


options_storage() ->
  Conf=tp:get_config(),
  [
   %% {Name,   ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
   {host,      $h,        "host",       {string,
                                          proplists:get_value(host,Conf,"httpsi://localhost:49800/")},
    "tpnode's base address, use httpsi as protocol for ssl without cert verification"},
   {rawkey,    undefined,  "hexkey",    string, "keyfile"},
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
