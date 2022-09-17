-module(ldb).

-export([get/2]).
-export([put/3]).

write(Filename, KVS) ->
  Bin=lists:foldl(
    fun({K,V},A) ->
        [A,hex:encode(binary:encode_unsigned(K))," ",
         hex:encode(binary:encode_unsigned(V)),"\n"]
    end, [], KVS),
  file:write_file(Filename, Bin).

consult(Filename) ->
  case file:read_file(Filename) of
    {ok, Bin} ->
      {ok, [parsef(Bin)]};
    Other ->
      Other
  end.

parsef(Bin) ->
  lists:filtermap(
    fun(L) ->
        case binary:split(L,<<" ">>) of
          [K,V] ->
            {true,{binary:decode_unsigned(hex:decode(K)),
                   binary:decode_unsigned(hex:decode(V))
                  }};
          _ ->
            false
        end
    end,
    binary:split(Bin,<<"\n">>,[global])
   ).

put(Address, code, Code) ->
  BA=naddress:encode(Address),
  Filename=list_to_binary([BA,"_storage.txt"]),
  file:write_file(Filename, Code);

put(Address, storage, KeyVal) ->
  BA=naddress:encode(Address),
  Filename=list_to_binary([BA,"_storage.txt"]),
  Old=case consult(Filename) of
        {ok, [PL]} ->
          PL;
        {error, _} ->
          []
      end,
  R=lists:foldl(
    fun({K,0}, Acc) ->
        lists:keydelete(K,1,Acc);
       ({K,V}, Acc) ->
        [{K,V}|lists:keydelete(K,1,Acc)]
    end, Old, KeyVal),
  %file:write_file(Filename, io_lib:format("~p.~n",[R])).
  write(Filename, R).

get(Address, code) ->
  BA=naddress:encode(Address),
  Filename=list_to_binary([BA,"_code.bin"]),
  case file:read_file(Filename) of
    {ok, Bin} ->
      Bin;
    {error, _} ->
      <<>>
  end;

get(Address, {storage, Key}) ->
  BA=naddress:encode(Address),
  Filename=list_to_binary([BA,"_storage.txt"]),
  case consult(Filename) of
    {ok, [PL]} ->
      proplists:get_value(Key,PL,0);
    {error, _Er} ->
      0
  end.

