-module(transpose).
-export([transpose/1, transpose/2]).

transpose(Map) ->
    transpose(Map, []).

transpose(Map, Opts) ->
  maps:fold(
    fun(Key, Value, Acc) ->
        case lists:member(no_list, Opts) of
          true ->
            maps:put(Value, [Key|maps:get(Value, Acc, [])], Acc);
          false ->
            lists:foldl(
              fun(Key1, Acc1) ->
                  maps:put(Key1, [Key|maps:get(Key1, Acc1, [])], Acc1)
              end, Acc, Value)
        end
    end, #{}, Map).

