-module(tp_readfile).
-export([readfile/1,parse_code/1]).

readfile(FFilename) ->
    case file:read_file(FFilename) of
        {ok, CBin} ->
            CBin;
        {error, Reason} ->
            io:format("Can't read file ~s: ~p~n",
                      [FFilename, Reason]),
            throw('badfile')
    end.

parse_code(<<"0x",Hex/binary>>) ->
    hex:decode(Hex);
parse_code(<<"hex:",Hex/binary>>) ->
    hex:decode(Hex);
parse_code(<<"b64:",Hex/binary>>) ->
    base64:decode(Hex);
parse_code(<<"raw:",Hex/binary>>) ->
    Hex;
parse_code(<<"hex@",CodeFilename/binary>>) ->
    Data=readfile(CodeFilename),
    hex:decode(string:chomp(Data));
parse_code(<<"b64@",CodeFilename/binary>>) ->
    base64:decode(string:chomp(readfile(CodeFilename)));
parse_code(<<"raw@",CodeFilename/binary>>) ->
    readfile(CodeFilename);
parse_code(Code) ->
    base64:decode(string:chomp(Code)).

