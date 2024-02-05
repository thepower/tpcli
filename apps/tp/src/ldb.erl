-module(ldb).

-export([get/2]).
-export([put/3]).
-export([storage_get/2]).
-export([storage_put/2]).

encode_kv(<<B:32/binary>>) ->
  hex:encode(B);
encode_kv(Integer) ->
  hex:encode(<<Integer:256/big>>).

write(Filename, KVS) ->
  Bin=lists:foldl(
    fun({K,V},A) ->
        [A,
         encode_kv(K)," ",
         encode_kv(V),"\n"
         %,hex:encode(binary:encode_unsigned(K))," ", hex:encode(binary:encode_unsigned(V)),"\n"
        ]
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

put(Address, storage, KeyVal) ->
  BA=hex:encode(Address),
  Filename=list_to_binary(["ldb/",BA,"_storage.txt"]),
  Old=case consult(Filename) of
        {ok, [PL]} ->
          PL;
        {error, _} ->
          []
      end,
  R=lists:foldl(
    fun({K,0}, Acc) ->
        %case lists:keyfind(K,1,Acc) of %for debug
        %  false -> ok;
        %  {_,OVal} ->
        %    %io:format("~s:~s => del (before ~p)~n",[BA,hex:encode(binary:encode_unsigned(K)),OVal])
        %end,
        lists:keydelete(K,1,Acc);
       ({K,V}, Acc) ->
        %case lists:keyfind(K,1,Acc) of %for debug
        %  false ->
        %    %io:format("~s:~s => ~p~n",[BA,hex:encode(binary:encode_unsigned(K)),V]);
        %  {_,OVal} when OVal =/= V ->
        %    %io:format("~s:~s => ~p (before ~p)~n",[BA,hex:encode(binary:encode_unsigned(K)),V,OVal]);
        %  _ ->
        %    ignore
        %end,
        [{K,V}|lists:keydelete(K,1,Acc)]
    end, Old, KeyVal),
  %file:write_file(Filename, io_lib:format("~p.~n",[R])).
  if(Old==R) ->
      ok;
    true ->
      write(Filename, R)
  end;

put(Address, Type, Code) when is_atom(Type) ->
  BA=hex:encode(Address),
  Filename= << "ldb/", BA/binary,"_",(atom_to_binary(Type))/binary,".bin" >>,
  file:write_file(Filename, Code).

get(Address, {storage, Key}) ->
  %T=erlang:system_time(),
  BA=hex:encode(Address),
  Filename=list_to_binary(["ldb/",BA,"_storage.txt"]),
  case consult(Filename) of
    {ok, [PL]} ->
      V=proplists:get_value(Key,PL,0),
      %io:format("~s:~p = ~p~n",[BA,hex:encode(binary:encode_unsigned(Key)),V]),
      T1=erlang:system_time(),
      %io:format("time ~wns~n",[T1-T]),
      V;
    {error, _Er} ->
      0
  end;

get(Address, abi) ->
  BA=hex:encode(Address),
  Filename= << "ldb/",BA/binary,"_abi.json" >>,
  try
    contract_evm_abi:parse_abifile(Filename)
  catch _Ec:_Ee ->
          []
  end;

get(Address, Type) when is_atom(Type) ->
  BA=hex:encode(Address),
  Filename= << "ldb/",BA/binary,"_",(atom_to_binary(Type))/binary,".bin" >>,
  case file:read_file(Filename) of
    {ok, Bin} ->
      Bin;
    {error, _} ->
      <<>>
  end.


storage_get(Address, Key) ->
  BA=binary_to_list(hex:encode(Address)),
  %io:format("read ~s ~w~n",[BA,Key]),
  Filename=lists:flatten(["ldb/",BA,"_storage.txt"]),
  case file:open(Filename, [read, raw, {read_ahead, 65536}, binary]) of
    {error,enoent} ->
      0;
    {ok,FD} ->
      file:position(FD,0),
      R=flo_read(FD,encode_kv(Key)),
      ok=file:close(FD),
      R
  end.

flo_read(FD, Key) ->
  case file:read_line(FD) of
    {ok,<<Key1:(size(Key))/binary,32,V/binary>>} when Key==Key1 ->
      binary:decode_unsigned(hex:decode(string:chomp(V)));
    {ok,_Any} ->
      flo_read(FD,Key);
    eof ->
      0;
    {error, Reason} ->
      throw({error, Reason})
  end.

storage_put(Address, KVs) ->
  BA=binary_to_list(hex:encode(Address)),
  Filename=lists:flatten(["ldb/",BA,"_storage.txt"]),
  {ok,FD}=file:open(Filename, [read, write, raw, binary]),
  KVBs=lists:map(
         fun({K,V}) ->
             {encode_kv(K),encode_kv(V)}
         end, KVs),
  R1=read_and_replace(FD,KVBs),
  file:position(FD,eof),
  lists:foreach(
    fun({K,V}) ->
        ok=file:write(FD,[K,32,V,"\n"])
    end,
    R1),
  file:close(FD),
  R1.

read_and_replace(FD, KVs) ->
  case file:read(FD,130) of
    {ok,<<K:64/binary," ",V:64/binary,"\n">>} ->
      case lists:keyfind(K,1,KVs) of
        {K,Val1} ->
          if(Val1==V) ->
              read_and_replace(FD,KVs--[{K,Val1}]);
            true ->
              {ok,P0}=file:position(FD,cur),
              {ok,_}=file:position(FD,{cur,-65}),
              ok=file:write(FD,Val1),
              {ok,_}=file:position(FD,P0),
              read_and_replace(FD,KVs--[{K,Val1}])
          end;
        false ->
          read_and_replace(FD,KVs)
      end;
    eof ->
      KVs
  end.

