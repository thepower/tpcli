-module(tp_keystore).

-export([generate/4,
         get_key/3
        ]).

ks_path() ->
  case os:getenv("TPCLI_KEYSTORE") of
    false ->
      "/tmp";
    Path ->
      Path
  end.

crypt(undefined, Data, _Enc) ->
  Data;

crypt(Password, Data, Enc) ->
  <<IV:16/binary,EncKey/binary>> = crypto:hash(sha256, Password),
  crypto:crypto_one_time(aes_128_cbc, EncKey, IV, Data, [{encrypt,Enc}]).

get_key(App, Name, Password) ->
  case file:consult(filename:join([ks_path(), App, [Name,".key"]])) of
    {ok, Data} ->
      case lists:keyfind(epriv,1,Data) of
        {epriv, Enc} ->
          Pub=hex:decode(proplists:get_value(pubkey, Data)),
          Dec=crypt(Password, base64:decode(Enc), false),
          PubKey2 = try
                      tpecdsa:calc_pub(Dec)
                    catch _:_ -> false end,
          if(Pub==PubKey2) ->
              {ok,{Dec,Data}};
            true ->
              {error, invalid_password}
          end;
        false ->
          Dec=hex:decode(proplists:get_value(privkey, Data)),
          if Password == undefined ->
              {ok,{Dec,Data}};
            true ->
              {error, invalid_password}
          end
      end;
    {error, _} ->
      {error, not_found}
  end.

generate(App, Kind, Name, Password) ->
    NewKey  = tpecdsa:generate_priv(Kind),
    KeyFile = filename:join([ks_path(), App, [Name,".key"]]),
    case filelib:is_file(KeyFile) of
      true ->
        {error, exists};
      false ->
        case filelib:ensure_dir(KeyFile) of
          ok ->
            PubKey  = tpecdsa:calc_pub(NewKey),
            EK=case Password of
                 X when is_binary(X) ->
                   {epriv,base64:encode(crypt(Password, NewKey, true))};
                 undefined ->
                   {privkey,hex:encode(NewKey)}
               end,
            Exp=[{pubkey,hex:encode(PubKey)}, EK],
            ok=file:write_file( KeyFile, [ io_lib:format("~p.~n",[E]) || E <- Exp ]),
            {ok, {NewKey, Exp}};
          {error, _} ->
            {error, dir_error}
        end
    end.



