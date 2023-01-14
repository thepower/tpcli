-module(tp_http_handler).

-export([start/0]).
-export([init/2]).

init(Req0=#{method := <<"POST">>, has_body := true, path := <<"/tp">>} ,State) ->
  {ok, Body, Req1} = cowboy_req:read_body(Req0),
  J=jsx:decode(Body,[return_maps]),
  if(is_list(J)) ->
      try
        R=handle_req(J, Req0, #{}),
        ResH=maps:get(headers,R,#{}),
        case maps:is_key(error, R) of
          true ->
            Req = cowboy_req:reply(500 , ResH, maps:get(error,R), Req1),
            {ok,Req,State};
          false ->
            Req = cowboy_req:reply(200 , ResH, maps:get(body,R,<<>>), Req1),
            {ok,Req,State}
        end
      catch throw:{err,Reason} ->
              {ok,
               cowboy_req:reply(500, #{}, list_to_binary(io_lib:format("~s",[Reason])), Req0),
               State}
      end;
    true ->
      Req = cowboy_req:reply(400, #{}, <<"List expected in JSON">>, Req0),
      {ok,Req,State}
  end;

init(Req0=#{path := <<"/tp">>} ,State) ->
  Req = cowboy_req:reply(400, #{}, <<"Only POST supported at this endpoint with JSON in body">>, Req0),
  {ok,Req,State};

init(Req0 ,State) ->
  Req = cowboy_req:reply(404, #{}, <<"Not found">>, Req0),
  {ok,Req,State}.

start() ->
  Dispatch = cowboy_router:compile([ {'_' ,[{"/tp",?MODULE, []} ]} ]),
  {ok,_} = cowboy:start_clear(http_listener,
                              [{port,8086}],
                              #{env => #{dispatch => Dispatch}}
                             ).

handle_req([#{<<"a">>:= <<"genkey">>,<<"k">>:=Kind}|Rest], Req, A) ->
  K = case Kind of
        <<"ed25519">> -> ed25519;
        <<"secp256k1">> -> secp256k1
      end,
  Key=tpecdsa:generate_priv(K),
  handle_req(Rest,Req,A#{body=>Key, key=>Key});

handle_req([#{<<"a">>:= <<"ksgenkey">>,
              <<"k">>:=Kind,
              <<"app">>:=AppID,
              <<"name">>:=Name}=Q|Rest], Req, A) ->
  K = case Kind of
        <<"ed25519">> -> ed25519;
        <<"secp256k1">> -> secp256k1
      end,
  case tp_keystore:generate(AppID, K, Name, maps:get(<<"pw">>,Q,undefined)) of
    {ok, {Key,_Extra}} ->
      Pub  = tpecdsa:calc_pub(Key),
      handle_req(Rest,Req,A#{body=>Pub, key=>Key});
    {error, Reason} ->
      Resp=list_to_binary( io_lib:format("key generation failed: ~s", [Reason])),
      throw({err,Resp})
  end;

handle_req([#{<<"a">>:= <<"construct">>,<<"d">>:=Data}|Rest], Req, A) ->
  Tx=tx:construct_tx(tp_construct:construct(Data)),
  handle_req(Rest,Req,A#{body=>tx:pack(Tx), tx=>Tx});

handle_req([#{<<"a">>:= <<"kssign">>,
              <<"app">>:=AppID,
              <<"name">>:=Name
             }=Q|Rest], Req, #{tx:=Tx}=A) ->
  case tp_keystore:get_key(AppID, Name, maps:get(<<"pw">>,Q,undefined)) of
    {ok,{Key, _Extra}} ->
      Tx1=tx:sign(Tx,Key),
      handle_req(Rest, Req, A#{body=>tx:pack(Tx1), tx=>Tx1});
    {error, Reason} ->
      throw({err,Reason})
  end;

handle_req([#{<<"a">>:= <<"sign">>, <<"k">>:=Key0}|Rest], Req, #{tx:=Tx}=A) ->
  Key=case Key0 of
        <<"0x",Hex/binary>> -> hex:decode(Hex);
        _ -> base64:decode(Key0)
      end,
  Tx1=tx:sign(Tx,Key),
  handle_req(Rest, Req, A#{body=>tx:pack(Tx1), tx=>Tx1});

handle_req([#{<<"a">>:= <<"showtx">>}|Rest], Req, #{tx:=B}=A) ->
  handle_req(Rest, Req, A#{body=>["{\n",tp_show:txl(B),"\n}\n"]});

handle_req([#{<<"a">>:= <<"base64">>}|Rest], Req, #{body:=B}=A) ->
  handle_req(Rest, Req, A#{body=>base64:encode(B)});

handle_req([#{<<"a">>:= <<"hex">>}|Rest], Req, #{body:=B}=A) ->
  handle_req(Rest, Req, A#{body=>["0x",hex:encode(B)]});

handle_req([#{<<"a">>:= Any}], _, _A) ->
  throw({err,["unknown action ",Any]});

handle_req([], _, A) ->
  A.

