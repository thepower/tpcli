-module(tp_manifest).
-export([dir2manifest/1]).
-export([parse_manifest/1]).

parse_manifest(Bin) ->
  Hash = crypto:hash(sha256,Bin),
  J=jsx:decode(Bin,[return_maps]),
  {Count, Size, Files}=lists:foldl(
    fun(#{<<"hash">>:=FH,<<"name">>:=FN,<<"path">>:=FP,<<"size">>:=FS},
        {Cnt,
         Siz,
         Lst
        }) ->
        {
         Cnt+1,
         Siz+FS,
         [{binary_to_list(FN),hex:decode(FH),binary_to_list(FP),FS}|Lst]
        }
    end, {0,0,[]}, J),
  #{
    hash => Hash,
    files_count => Count,
    size => Size,
    files => Files
   }.


dir2manifest(Path) ->
  {Size,Files,Lst}=lists:foldl(
               fun(Filename0,{S,C,A}) ->
                   Filename=filename:join([Path,Filename0]),
                   case filelib:is_regular(Filename) of
                     false ->
                       {S,C,A};
                     true ->
                       {ok, Bin} = file:read_file(Filename),
                       Size=size(Bin),
                       {S+Size,
                        C+1,
                        [#{name => list_to_binary(filename:basename(Filename0)),
                           path => list_to_binary(filename:dirname(Filename0)),
                           size => Size,
                           hash => hex:encode(crypto:hash(sha256,Bin))}|A]
                       }
                   end
               end,
               {0,0,[]},
               filelib:wildcard("**",Path)
              ),
  JSON=jsx:encode(Lst),
  {ok, #{
         manifest => JSON,
         size => Size,
         files_count => Files,
         hash => crypto:hash(sha256,JSON)
        }
  }.

