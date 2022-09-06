-module(tp_abi).
-export([parse_abifile/1]).
-export([find_function/2]).
-export([decode_abi/2]).
-export([decode_abi_map/2]).
-export([encode_simple/1]).

decode_abi(Bin,Args) ->
  decode_abi(Bin,Args,Bin,[]).

decode_abi_map(Bin,Args) ->
  maps:from_list(decode_abi(Bin,Args,Bin,[])).

decode_abi(_,[],_Bin,Acc) ->
  {_,List}=lists:foldl(
    fun({Name,_Type,Value},{N,A}) ->
        {N-1,[
              {
               if(Name == <<>>) ->
                  <<"arg",(integer_to_binary(N))/binary>>;
                true ->
                  Name
              end,
               Value
              } | A ]}
    end,
    {length(Acc),[]},
    Acc
   ),
  List;

decode_abi(<<Ptr:256/big,RestB/binary>>,[{Name,string}|RestA],Bin,Acc) ->
  <<_:Ptr/binary,Len:256/big,Str:Len/binary,_/binary>> = Bin,
  decode_abi(RestB, RestA, Bin, [{Name, string, Str}|Acc]);
decode_abi(<<Val:256/big,RestB/binary>>,[{Name,address}|RestA],Bin,Acc) ->
  decode_abi(RestB, RestA, Bin, [{Name, address, Val}|Acc]);
decode_abi(<<Val:256/big,RestB/binary>>,[{Name,uint256}|RestA],Bin,Acc) ->
  decode_abi(RestB, RestA, Bin, [{Name, uint256, Val}|Acc]).


find_function(ABI, Sig) ->
  [Name|_Args]=string:tokens(Sig,"(,)"),
  lists:keyfind({function,list_to_binary(Name)},1,ABI).


parse_abifile(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  JSON= jsx:decode(Bin,[return_maps]),
  ABI = lists:filtermap(
           fun parse_item/1,
           JSON
          ),
  ABI.

parse_item(#{
             <<"outputs">> := O,
             <<"name">> := Name,
             <<"inputs">> := I,
             <<"type">> := <<"function">>
  %{<<"stateMutabil"...>>,<<"view">>},
  %{<<"type">>,<<"func"...>>}
            }) ->
  {true,{{function,Name},convert_io(I),convert_io(O)}};
parse_item(#{}) ->
  false.

convert_io(List) ->
  lists:map(
    fun(#{
          <<"name">> := Name,
          <<"type">> := Type}) ->
        {Name, convert_type(Type)}
    end, List).


convert_type(<<"string">>) -> string;
convert_type(<<"address">>) -> address;
convert_type(<<"uint256">>) -> uint256.


encode_simple(Elements) ->
  HdLen=length(Elements)*32,
  {H,B,_}=lists:foldl(
            fun(E, {Hdr,Body,BOff}) when is_integer(E) ->
                {<<Hdr/binary,E:256/big>>,
                 Body,
                 BOff};
               ({bin, <<E:256/big>>}, {Hdr,Body,BOff}) ->
                {<<Hdr/binary,E:256/big>>,
                 Body,
                 BOff};
               (E, {Hdr,Body,BOff}) when is_binary(E) ->
                EncStr=encode_str(E),
                {
                 <<Hdr/binary,BOff:256/big>>,
                 <<Body/binary,EncStr/binary>>,
                 BOff+size(EncStr)
                }
            end, {<<>>, <<>>, HdLen}, Elements),
  HdLen=size(H),
  <<H/binary,B/binary>>.

encode_str(Bin) ->
  Pad = case (size(Bin) rem 32) of
          0 -> 0;
          N -> 32 - N
        end*8,
  <<(size(Bin)):256/big,Bin/binary,0:Pad/big>>.

