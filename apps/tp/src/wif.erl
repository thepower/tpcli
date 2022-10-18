-module(wif).

-export([encodekey/1, parsekey/1]).

encodekey(Pvt) ->
    H2= <<128, Pvt/binary, 1>>,
    <<H3:4/binary, _/binary>>=crypto:hash(sha256, crypto:hash(sha256, H2)),
    base58:encode(<<H2/binary, H3/binary>>).

parsekey(<<"0x", BKey/binary>>) ->
    hex:parse(BKey);
parsekey(Base58) ->
    B58Decode=base58:decode(Base58),
    KS=size(B58Decode)-5,
    case B58Decode of
        <<128, KeyBody:KS/binary, KC:4/binary>> ->
            <<H3:4/binary, _/binary>>=
            crypto:hash(sha256,
                        crypto:hash(sha256, <<128:8/integer, KeyBody/binary>>)
                       ),
            if(KC==H3 andalso KS==32) ->
                  KeyBody;
              (KC==H3 andalso KS==33) ->
                  <<KB:32/binary, _:1/binary>>=KeyBody,
                  KB;
              true ->
                  error
            end;
        _ ->
            error
    end.


