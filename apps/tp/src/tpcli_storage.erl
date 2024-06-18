-module(tpcli_storage).

-export([main_run/2]).

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
                 ({baseurl,_}) -> true;
                 ({stask,_}) -> true;
                 ({bucketname,_}) -> true;
                 ({mkmanifest,_}) -> true;
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

run([{stask,TaskID}|Rest], Opt) ->
  Addr=case proplists:get_value(address,Opt) of
         undefined ->
           throw('address_required');
         A ->
           naddress:decode(A)
       end,

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
      io:format("Example for manifest.json: ~s/~w~n",[UURL,TaskID]),
      io:format("Example for any other file: ~s/~w/filename.ext~n",[UURL,TaskID]),
  run(Rest, Opt);

run([{bucketname,TBucket}|Rest], Opt) ->
  Addr=case proplists:get_value(address,Opt) of
         undefined ->
           throw('address_required');
         A ->
           naddress:decode(A)
       end,
  TInterval=proplists:get_value(interval,Opt),
  FManifest=proplists:get_value(manifest, Opt),
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
  Node=proplists:get_value(host,Opt),
  {Seq,Am} = case tpapi2:ledger(Node, MyAddr) of
               {ok,#{<<"seq">>:=Seq1,<<"amount">>:=Am1}} -> {Seq1, Am1};
               {ok,#{<<"amount">>:=Am1}} -> {0, Am1};
               Err ->
                 logger:notice("Can't get ledger for address ~s: ~p",[naddress:encode(MyAddr), Err]),
                 {0,#{}}
             end,
  
  io:format("Addr ~s bucket ~p exp ~p manifest ~w hash ~s~n",
            [naddress:encode(Addr),TBucket, Expire, Size, hex:encode(Hash)]),

  Transfer=case proplists:get_value(transfer,Opt) of
             undefined ->
               [#{purpose=>transfer, amount=>1, cur=><<"SK">>}];
             "0" ->
               [];
             Str ->
               [TAm,TCur] = string:split(Str," "),
               [#{purpose=>transfer, amount=>list_to_integer(TAm), cur=>list_to_binary(TCur)}]
           end,

  Gas=case proplists:get_value(gas,Opt) of
             undefined ->
               undefined;
             "0" ->
               [];
             GStr ->
               [GTAm,GTCur] = string:split(GStr," "),
               [#{purpose=>gas, amount=>list_to_integer(GTAm), cur=>list_to_binary(GTCur)}]
           end,

  GasFun=fun(RequiredGas, TxBytes) ->
             GasPrice=tpapi2:gas_price(RequiredGas,Node),
             TG=case tpapi2:match_cur(GasPrice, Am) of
                  undefined ->
                    Transfer;
                  {GasAm, GasCur} when Gas==undefined ->
                    [#{purpose=>gas, amount=>GasAm, cur=>GasCur} |Transfer];
                  {_,_} ->
                    Gas++Transfer
                end,
             Fee=tpapi2:fee_price(TxBytes,Node),
             case tpapi2:match_cur(Fee, Am) of
               undefined ->
                 TG;
               {FeeAm, FeeCur} ->
                 [#{purpose=>srcfee, amount=>FeeAm, cur=>FeeCur} |TG]
             end
         end,

  Tx=tp_storage:new_task(
       Addr,
       MyAddr,
       Seq,
       TBucket,
       Hash,
       Expire,
       Size,
       GasFun),
  SubmitFun=fun({ok,#{<<"ok">> := true, <<"retval">>:=R}}) ->
                io:format("~nYour task ID: ~w~n~n",[R]),
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
                io:format("manifest.json upload path: ~s/~w~n",[UURL,R]),
                io:format("files upload path: ~s/~w/file~n",[UURL,R]),
                ok;
               (Any) ->
                io:format("Unexpected res ~p~n",[Any]),
                error
            end,

  run(Rest, [{submitcb,SubmitFun},{tx,Tx}|Opt]);

run([{baseurl, URL}|Rest], Opt) ->
  Addr=case proplists:get_value(address,Opt) of
         undefined ->
           throw('address_required');
         A ->
           naddress:decode(A)
       end,
  {BaseURL,UploadURL} = case string:split(URL,",") of
                    [BURL] ->
                      {BURL,
                       lists:flatten([BURL,"/upload"])
                      };
                    [BURL, UURL] ->
                      {BURL, UURL}
                  end,

  File=proplists:get_value(keyfile, Opt),
  Prev=readkey(File),
  MyAddr=case proplists:get_value(address, Prev) of
           undefined ->
             throw('no_address');
           Adr ->
             naddress:decode(Adr)
         end,
  Node=proplists:get_value(host,Opt),
  {ok,#{<<"seq">>:=Seq,<<"amount">>:=Am}}=tpapi2:ledger(Node, MyAddr),

  GasFun=fun(RequiredGas, _TxBytes) ->
             Prices=tpapi2:gas_price(RequiredGas,Node),
             if(size(Prices)==0) ->
                 [];
               true ->
                 {DC,DAm}=hd(maps:to_list(Prices)),
                 [
                  maps:fold(
                    fun(Cur,Amount,Acc) ->
                        CAm=maps:get(Cur,Am,0),
                        if CAm>=Amount ->
                             #{purpose=>gas, amount=>Amount, cur=>Cur};
                           true ->
                             Acc
                        end
                    end, 
                    #{purpose=>gas, amount=>DAm, cur=>DC},
                    Prices)
                 ]
             end
         end,
  io:format("Addr ~s upload url ~s base url ~s~n",
                [naddress:encode(Addr), UploadURL, BaseURL]),
  Tx=tp_storage:new_prov(Addr, MyAddr, Seq, UploadURL, BaseURL, GasFun),
  run(Rest, [{tx,Tx}|Opt]);

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

run([showtx|Rest], Opt) ->
  Tx=proplists:get_value(tx,Opt),
  if(Tx==undefined) ->
      throw('no_tx_for_show');
    true ->
      tp_show:tx(Tx)
  end,
  run(Rest, Opt);

run([sign|Rest], Opt) ->
  run(Rest, tpcli_main:sign(Opt));

run([submit|Rest], Opt) ->
  run(Rest, tpcli_main:submit(Opt));

run([estimate|Rest], Opt) ->
  run(Rest, tpcli_main:estimate(Opt));

run([ss|Rest], Opt) ->
  run(Rest, tpcli_main:submit(tpcli_main:sign(Opt)));
  
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
  FManifest=proplists:get_value(manifest, Opt),
  io:format("Files found: ~w~n",[C]),
  io:format("Manifest writtent to ~s~n",[FManifest]),
  io:format("Manifest hash: ~s~n",[hex:encode(Hash)]),
  io:format("Total files size: ~w~n",[Size]),
  ok=file:write_file(FManifest,JSON),
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

