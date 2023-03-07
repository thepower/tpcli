-module(tp_abi).
-export([parse_abifile/1]).
-export([find_function/2]).
-export([decode_abi/2]).
-export([decode_abi_map/2]).
-export([encode_simple/1]).
-export([encode_params/2]).

decode_abi(Bin,Args) ->
  decode_abi(Bin,Args,Bin,[]).

decode_abi_map(Bin,Args) ->
  maps:from_list(decode_abi(Bin,Args,Bin,[])).

decode_abi(_,[],_Bin,Acc) ->
  {_,List}=lists:foldl(
    fun({Name,_Type,Value},{N,A}) ->
        {N-1,[{
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
decode_abi(<<Val:256/big,RestB/binary>>,[{Name,address}|RestA],Bin,Acc)
  when Val > 9223372036854775808 andalso Val < 13835058055282163712 ->
  decode_abi(RestB, RestA, Bin, [{Name, address, binary:encode_unsigned(Val)}|Acc]);
decode_abi(<<Val:256/big,RestB/binary>>,[{Name,address}|RestA],Bin,Acc) ->
  decode_abi(RestB, RestA, Bin, [{Name, address, Val}|Acc]);
decode_abi(<<Val:256/big,RestB/binary>>,[{Name,uint256}|RestA],Bin,Acc) ->
  decode_abi(RestB, RestA, Bin, [{Name, uint256, Val}|Acc]).

find_function(ABI, Sig) when is_list(ABI), is_list(Sig) ->
  logger:notice("deprecated clause of find_function called"),
  find_function(list_to_binary(Sig), ABI);

find_function(Sig, ABI) when is_binary(Sig), is_list(ABI) ->
  {Name,Args} = fspl(Sig),
  lists:filter(
    fun({{function,LName},CS,_}) when LName == Name ->
        [ Type || {_,Type} <- CS ] == Args;
       (_) ->
        false
    end,
    ABI).

fspl(Signature) ->
  {match,[Fun,Args]} = re:run(
                         Signature,
                         "^([01-9a-zA-Z_]+)\\\((.*)\\\)",
                         [{capture, all_but_first, binary}]),
  {Fun,
   [ convert_type(T) || T<- binary:split(Args,<<",">>,[global]), size(T)>0]
  }.

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

encode_type(<<Input:256/big>>, uint256) ->
  <<Input:256/big>>;
encode_type(Input, uint256) when is_integer(Input) ->
  <<Input:256/big>>;
encode_type(Input, address) when is_integer(Input) ->
  <<Input:256/big>>;
encode_type(Input, address) ->
  IVal=binary:decode_unsigned(Input),
  <<IVal:256/big>>;
encode_type(_, Type) ->
  throw({'unexpected_type',Type}).


encode_params(D,T) ->
  if length(D)>length(T) -> throw('unexpected_arg');
     length(T)>length(D) -> throw('more_arg_expected');
     true ->
       HdLen=length(T)*32,
       encode_typed(D,T,<<>>,<<>>,HdLen)
  end.


encode_typed([],[], Hdr, Body, _BOff) ->
  <<Hdr/binary,Body/binary>>;

encode_typed([Val|RVal],[{_Name,string}|RType], Hdr, Body, BOff) ->
  EncStr=encode_str(Val),
  encode_typed(RVal,
               RType,
               <<Hdr/binary,BOff:256/big>>,
               <<Body/binary,EncStr/binary>>,
               BOff+size(EncStr));

encode_typed([Val|RVal],[{_Name,Type}|RType], Hdr, Body, BOff) ->
  case encode_type(Val, Type) of
    Bin when is_binary(Bin) ->
      encode_typed(RVal,
                   RType,
                   <<Hdr/binary,Bin/binary>>,
                   Body,
                   BOff);
    {body,Bin} when is_binary(Bin) ->
      encode_typed(RVal,
               RType,
               <<Hdr/binary,BOff:256/big>>,
               <<Body/binary,Bin/binary>>,
               BOff+size(Bin))
  end.

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

encode_str(List) when is_list(List) ->
  encode_str(list_to_binary(List));

encode_str(Bin) ->
  Pad = case (size(Bin) rem 32) of
          0 -> 0;
          N -> 32 - N
        end*8,
  <<(size(Bin)):256/big,Bin/binary,0:Pad/big>>.

