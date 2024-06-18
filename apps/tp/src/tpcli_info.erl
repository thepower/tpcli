-module(tpcli_info).

-export([main_run/2,run/2]).

main_run(Options, NonOpt) ->
  logger:info("Run as ~p~n",[Options]),
  {Act,Opt}=lists:partition(
              fun(A) when is_atom(A) -> true;
                 (_) -> false
              end,Options),
  try
    application:ensure_all_started(tp),
    logger:info("Run ~p~n",[Act]),
    Opts=[{extra_arg,NonOpt}|Opt],
    logger:info("Opts ~p~n",[Opts]),
    run(Act, Opts)
  catch throw:Atom:S ->
          io:format(standard_error,"\nError: ~p @ ~p\n",[Atom,S])
  end.

run([], _) ->
  done;

run([{gasprice, N}|Rest], Opt) ->
  Gas=tpapi2:gas_price(N,proplists:get_value(host,Opt)),
  io:format("~p~n",[Gas]),
  run(Rest, Opt);


run([nodes|Rest], Opt) ->
  Chain=proplists:get_value(chain, Opt, 0),
  logger:info("Candidates for chain ~p~n",[Chain]),
  case tpapi2:httpget(
         proplists:get_value(host,Opt),
         "/api/nodes/"++integer_to_list(Chain)++".mp"
        ) of
    #{<<"chain_nodes">> := Nodes,
      <<"ok">> := true} ->
      Nodes1=maps:map(
               fun(_NodeID, #{<<"ip">> := IPS}) ->
                   lists:filter(
                     fun(IP) ->
                         case uri_string:parse(IP) of
                           #{scheme := <<"http">>} ->
                             true;
                           _ -> false
                         end
                     end,IPS)
                     end, Nodes),
      io:format("Nodes:~n~p~n",[Nodes1]),
      run(Rest, [{ret,Nodes1}|Opt]);
    Other ->
      logger:error("Can't get chain ~w nodes from ~p: ~p",
                   [proplists:get_value(host,Opt),
                    Chain,
                    Other])
  end;


run([candidates|Rest], Opt) ->
  Chain=proplists:get_value(chain, Opt, 0),
  logger:info("Candidates for chain ~p~n",[Chain]),
  case tpapi2:httpget(
         proplists:get_value(host,Opt),
         "/api/nodes/"++integer_to_list(Chain)++".mp"
        ) of
    #{<<"chain_nodes">> := Nodes,
      <<"ok">> := true} ->
      Nodes1=maps:map(
               fun(_NodeID, #{<<"ip">> := IPS}) ->
                   lists:filter(
                     fun(IP) ->
                         case uri_string:parse(IP) of
                           #{scheme := <<"http">>} ->
                             true;
                           _ -> false
                         end
                     end,IPS)
                     end, Nodes),
      Candidates=maps:map(
                   fun(_NodeID, [URL|_]) ->
                       case catch tpapi2:httpget(URL,"/api/block_candidates.mp") of
                       #{<<"ok">> := true,
                         <<"blocks">> := Blocks} ->
                           Blocks;
                        _ ->
                            []
                       end
                   end, Nodes1),
      BLs=maps:fold(
            fun(_NN, Blocks, Acc) ->
                lists:foldl(
                  fun(#{<<"hash">> := <<B0:6/binary,_/binary>>, <<"header">> := Hdr}, Acc1) ->
                      B=hex:encode(B0),
                          maps:put(B, Hdr, Acc1)
                  end, Acc, Blocks)
            end, #{}, Candidates),
      BCs=maps:fold(
            fun(NN, Blocks, Acc) ->
                lists:foldl(
                  fun(#{<<"hash">> := <<B0:6/binary,_/binary>>}, Acc1) ->
                      B=hex:encode(B0),
                      case maps:is_key(B, Acc1) of
                        true ->
                          Lst=maps:get(B, Acc1),
                          maps:put(B, lists:sort([NN|Lst]), Acc1);
                        false ->
                          maps:put(B, [NN], Acc1)
                      end
                  end, Acc, Blocks)
            end, #{}, Candidates),

      io:format("Candidates for chain ~p~n",
            [ Nodes1 ]),
      io:format("Candidates BL~n~p~n",
            [ BLs ]),
      Res=transpose:transpose(
            maps:fold(
              fun(B, Nd, Acc) ->
                  #{<<"height">>:=Hei, <<"parent">>:= <<Par:6/binary,_/binary>>, <<"roots">>:=Roots}=maps:get(B, BLs),

                  Roots1=maps:fold(
                           fun
                             (<<"ledger_hash">>, <<LH:8/binary,_:56/binary>>, Acc1) ->
                               [<<"lh:",((LH))/binary,";">>|Acc1];
                             (<<"ledger_hash">>, <<LH:4/binary,_:28/binary>>, Acc1) ->
                               [<<"lh:",(hex:encode(LH))/binary,";">>|Acc1];
                             (<<"settings_hash">>, <<LH:4/binary,_/binary>>, Acc1) ->
                               [<<"sh:",(hex:encode(LH))/binary,";">>|Acc1];
                             (<<"entropy">>, <<LH:4/binary,_/binary>>, Acc1) ->
                               [<<"ent",(hex:encode(LH))/binary,";">>|Acc1];
                             (<<"tmp">>, <<TMP:64/big>>, Acc1) ->
                               [<<"tmp",(integer_to_binary(TMP))/binary,";">>|Acc1];
                             (<<"mean_time">>, <<TMP:16/binary>>, Acc1) ->
                               [<<"tme",(integer_to_binary(binary:decode_unsigned(hex:decode(TMP)) div 1000))/binary,";">>|Acc1];
                             (<<"mean_time">>, <<TMP:64/big>>, Acc1) ->
                               [<<"tme",(integer_to_binary(TMP div 1000))/binary,";">>|Acc1];
                             (_,<<>>, Acc1) ->
                               Acc1;
                             (K, V, Acc1) ->
                               io:format("Unkn root ~p ~p~n",[K,V]),
                               Acc1
                           end, [], Roots),
                  BL=list_to_binary(
                       io_lib:format("~w:~s:~s",[Hei, hex:encode(Par), Roots1])
                      ),
                  maps:put(BL, Nd, Acc)
              end, #{}, BCs),[no_list]),
      maps:map(
        fun(K, V) ->
            io:format("Nodes ~100p~n",
                      [ K ]),
            lists:foreach(
              fun(N) ->
                  io:format("   : ~s~n",[N])
              end, V)
        end, Res),
      run(Rest, [{ret,Res}|Opt]);
    Other ->
      logger:error("Can't get chain ~w nodes from ~p: ~p",
                   [proplists:get_value(host,Opt),
                    Chain,
                    Other])
  end;

run([fetch_genesis|Rest], Opt) ->
  R=tpapi2:httpget(
         proplists:get_value(host,Opt),
         "/api/binblock/genesis"
        ),
  #{hash:=Hash,
    header:=#{chain:=CN}
   }=Block=block:unpack(R),
  io:format("Found chain ~w genesis hash 0x~s~n",[CN,hex:encode(Hash)]),
  io:format("Genesis b64 hash ~s~n",[base64:encode(Hash)]),
  ok=file:write_file(
    ["genesis_",integer_to_list(CN),".txt"],
    io_lib:format("~p.~n",[Block])),
  run(Rest, Opt);


run([Other|Rest], Opt) ->
  logger:error("I don't know how to do ~p~n",[Other]),
  run(Rest, Opt).


