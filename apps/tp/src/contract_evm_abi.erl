-module(contract_evm_abi).

-export([parse_abifile/1]).
-export([find_function/2, find_event/2, find_event_hash/2]).
-export([all_events/1, mk_sig/1, mk_fullsig/1]).
-export([sig_events/1]).
-export([decode_abi/2]).
-export([decode_abi/3]).
-export([decode_abi/4]).
-export([encode_abi_call/2]).
-export([decode_abi_call/2]).
-export([decode_abi_call/3]).
-export([encode_simple/1]).
-export([parse_signature/1]).
-export([encode_abi/2]).
-export([parse_type/1]).
-export([sig32/1, keccak/1]).
-export([unwrap/1]).

-include_lib("eunit/include/eunit.hrl").

keccak(Data) ->
  case erlang:function_exported(ksha3,hash,2) of
    true ->
      {ok,H} = ksha3:hash(256,Data),
      H;
    false ->
%      case erlang:function_exported(esha3,keccak_256,1) of
%        true ->
          esha3:keccak_256(Data)
%        false ->
%          throw('keccak_hash_absend')
%      end
  end.


sig32(Signature) ->
  <<H:32/big,_/binary>> = keccak(Signature),
  H.

sig_events(ABI) ->
  [ begin
      S=mk_sig(E),
      {S,keccak(S),In}
    end|| E={{event,_},In,_} <- ABI ].

all_events(ABI) ->
  lists:filter(
    fun({{event,_},_,_}) -> true; (_) -> false end,
    ABI).

unwrap([{_,L}|Rest]) when is_list(L) ->
  [unwrap(L)|unwrap(Rest)];

unwrap([{_,L}|Rest]) ->
  [L|unwrap(Rest)];

unwrap([L|Rest]) ->
  [L|unwrap(Rest)];

unwrap([]) ->
  [].

%force_cast(address,X) when is_binary(X) ->
%  binary:decode_unsigned(X)
%  if(size(X)<2) ->
%      <<()
%  binary:decode_unsigned(X);
force_cast(uint256,X) when is_binary(X) ->
  binary:decode_unsigned(X);
force_cast(_,X) ->
  X.

decode_abi(Bin,Args,Indexed,ResFun) ->
  decode_abi(Bin,Args,Bin,[],Indexed,ResFun).
decode_abi(Bin,Args,Indexed) ->
  decode_abi(Bin,Args,Bin,[],Indexed,undefined).
decode_abi(Bin,Args) ->
  decode_abi(Bin,Args,Bin,[],[],undefined).

decode_abi(Bin1,Args,Bin2,Acc,Idx,ProcFun) ->
  {_,_,_,Acc2,_} = decode_abi_internal(Bin1,Args,Bin2,Acc,Idx,ProcFun),
  Acc2.

decode_abi_internal(RestB,[],Bin,Acc,Idx,ProcFun) ->
  List=lists:foldl(
         fun
           ({'_naked',_Type,Value},A) when ProcFun == undefined ->
             [Value|A];
           ({Name,_Type,Value},A) when ProcFun == undefined ->
             [{Name, Value}|A];
           ({'_naked',Type,Value},A) when is_function(ProcFun,3) ->
             [ProcFun('_naked',Type,Value)|A];
           ({Name,Type,Value},A) when is_function(ProcFun,3) ->
             [{Name, ProcFun(Name,Type,Value)}|A]
         end,
         [],
         Acc
        ),
  {RestB,[],Bin,List,Idx};

decode_abi_internal(RestB,[{Name, {indexed,Type}}|RestA],Bin,Acc,[N|Idx], ProcFun) ->
  decode_abi_internal(RestB, RestA, Bin, [{Name, Type, force_cast(Type, N)}|Acc],Idx, ProcFun);

decode_abi_internal(RestB, [{Name, {{fixarray,Size},Type}}|RestA],Bin,Acc,Idx, ProcFun) ->
  {RB2,[],_,Tpl,Idx1}=decode_abi_internal(RestB, [{'_naked',Type} || _ <- lists:seq(1,Size)],Bin,[],Idx, ProcFun),
  decode_abi_internal(RB2, RestA, Bin, [{Name, {fixarray,Size}, Tpl}|Acc],Idx1, ProcFun);

decode_abi_internal(<<Ptr:256/big,RestB/binary>>,[{Name, {darray,Type}}|RestA],Bin,Acc,Idx, ProcFun) ->
  <<_:Ptr/binary,Size:256/big,Data/binary>> = Bin,
  {_,[],_,Tpl,Idx1}=decode_abi_internal(Data,[{'_naked',Type} || _ <- lists:seq(1,Size)],Data,[],Idx, ProcFun),
  decode_abi_internal(RestB, RestA, Bin, [{Name, tuple, Tpl}|Acc],Idx1, ProcFun);

decode_abi_internal(<<Ptr:256/big,RestB/binary>>,[{Name, {tuple,TL}}|RestA],Bin,Acc,Idx, ProcFun) ->
  <<_:Ptr/binary,Tuple/binary>> = Bin,
  {_,[],_,Tpl,Idx1}=decode_abi_internal(Tuple,TL,Tuple,[],Idx, ProcFun),
  decode_abi_internal(RestB, RestA, Bin, [{Name, tuple, Tpl}|Acc],Idx1, ProcFun);

%% this is experemental
%decode_abi_internal(RestB,[{Name, {tuple,TL}}|RestA],Bin,Acc,Idx) ->
%  {RestB2,[],_,Tpl,Idx1}=decode_abi_internal(RestB,TL,Bin,[],Idx),
%  decode_abi_internal(RestB2, RestA, Bin, [{Name, tuple, Tpl}|Acc],Idx1);

decode_abi_internal(<<Ptr:256/big,RestB/binary>>,[{Name,bytes}|RestA],Bin,Acc,Idx, ProcFun) ->
  <<_:Ptr/binary,Len:256/big,Str:Len/binary,_/binary>> = Bin,
  decode_abi_internal(RestB, RestA, Bin, [{Name, bytes, Str}|Acc],Idx, ProcFun);

decode_abi_internal(<<Ptr:256/big,RestB/binary>>,[{Name,string}|RestA],Bin,Acc,Idx, ProcFun) ->
  <<_:Ptr/binary,Len:256/big,Str:Len/binary,_/binary>> = Bin,
  decode_abi_internal(RestB, RestA, Bin, [{Name, string, Str}|Acc],Idx, ProcFun);

decode_abi_internal(<<Val:256/big,RestB/binary>>,[{Name,address}|RestA],Bin,Acc,Idx, ProcFun)
  when Val > 9223372036854775808 andalso Val < 13835058055282163712 ->
  decode_abi_internal(RestB, RestA, Bin, [{Name, address, binary:encode_unsigned(Val)}|Acc],Idx, ProcFun);
decode_abi_internal(<<Val:256/big,RestB/binary>>,[{Name,address}|RestA],Bin,Acc,Idx, ProcFun) ->
  decode_abi_internal(RestB, RestA, Bin, [{Name, address, Val}|Acc],Idx, ProcFun);
decode_abi_internal(<<Val:32/binary,RestB/binary>>,[{Name,{bytes,32}}|RestA],Bin,Acc,Idx, ProcFun) ->
  decode_abi_internal(RestB, RestA, Bin, [{Name, {bytes,32}, Val}|Acc],Idx, ProcFun);
decode_abi_internal(<<Val0:32/binary,RestB/binary>>,[{Name,{bytes,N}}|RestA],Bin,Acc,Idx, ProcFun) ->
  <<_:(32-N)/binary,Val/binary>> = Val0,
  decode_abi_internal(RestB, RestA, Bin, [{Name, {bytes,N}, Val}|Acc],Idx, ProcFun);
decode_abi_internal(<<Val:256/big,RestB/binary>>,[{Name,bool}|RestA],Bin,Acc,Idx, ProcFun) ->
  decode_abi_internal(RestB, RestA, Bin, [{Name, bool, Val==1}|Acc],Idx, ProcFun);
decode_abi_internal(<<_:248,Val:8/big,RestB/binary>>,[{Name,uint8}|RestA],Bin,Acc,Idx, ProcFun) ->
  decode_abi_internal(RestB, RestA, Bin, [{Name, uint8, Val}|Acc],Idx, ProcFun);
decode_abi_internal(<<Val:256/big,RestB/binary>>,[{Name,uint32}|RestA],Bin,Acc,Idx, ProcFun) ->
  decode_abi_internal(RestB, RestA, Bin, [{Name, uint32, Val band 16#ffffffff}|Acc],Idx, ProcFun);
decode_abi_internal(<<Val:256/big,RestB/binary>>,[{Name,uint256}|RestA],Bin,Acc,Idx, ProcFun) ->
  decode_abi_internal(RestB, RestA, Bin, [{Name, uint256, Val}|Acc],Idx, ProcFun).

cmp_abi([],[]) -> true;
cmp_abi([],[_|_]) -> false;
cmp_abi([_|_],[]) -> false;
cmp_abi([{_,K}|A1],[{_,K}|A2]) ->
  cmp_abi(A1,A2);
cmp_abi({tuple,K1},{tuple,K2}) ->
  cmp_abi(K1,K2);
cmp_abi({{fixarray,N},K1},{{fixarray,N},K2}) ->
  cmp_abi(K1,K2);
cmp_abi({darray,K1},{darray,K2}) ->
  cmp_abi(K1,K2);
cmp_abi(E1,E2) when not is_list(E1) andalso not is_list(E2) andalso E1=/=E2 ->
  false;
cmp_abi([{_,K1}|A1],[{_,K2}|A2]) ->
  case cmp_abi(K1,K2) of
    true ->
      cmp_abi(A1,A2);
    false ->
      false
  end.

find_function(ABI, Sig) when is_list(ABI), is_list(Sig) ->
  logger:notice("deprecated clause of find_function called"),
  find_function(list_to_binary(Sig), ABI);

find_function(Sig, ABI) when is_binary(Sig), is_list(ABI) ->
  {Name,Args} = sig_split(Sig),
  lists:filter(
    fun({{function,LName},_CS,_}) when LName == Name andalso Args==undefined ->
        true;
       ({{function,LName},CS,_}) when LName == Name ->
        cmp_abi(Args,CS);
       (_) ->
        false
    end,
    ABI).

find_event(Sig, ABI) when is_binary(Sig), is_list(ABI) ->
  {Name,Args} = sig_split(Sig),
  lists:filter(
    fun
      ({{event,LName},_,_}) when LName == Name andalso Args==undefined ->
        true;
      ({{event,LName},CS,_}) when LName == Name ->
        [ Type || {_,Type} <- CS ] == Args;
       (_) ->
        false
    end,
    ABI).

find_event_hash(SigHash, ABI) when is_binary(SigHash), is_list(ABI) ->
  lists:filter(
    fun
      ({{event,_LName},_CS,_}=ABI1) ->
        SigHash==keccak(mk_sig(ABI1));
       (_) ->
        false
    end,
    ABI).

mk_sig([]) ->
  []; % convert multiple
mk_sig([{{error,_},_,_}=E|Rest]) ->
  [ mk_sig(E) | mk_sig(Rest) ];
mk_sig([{{event,_},_,_}=E|Rest]) ->
  [ mk_sig(E) | mk_sig(Rest) ];
mk_sig([{{function,_},_,_}=E|Rest]) ->
  [ mk_sig(E) | mk_sig(Rest) ];
mk_sig([_|Rest]) ->
  mk_sig(Rest);

mk_sig({{EventOrFunction,Name},CS,_}) when EventOrFunction == event;
                                           EventOrFunction == error;
                                           EventOrFunction == function ->
  list_to_binary([ Name, "(", mk_sig_arr(CS), ")" ]).

mk_sig_arr(CS) ->
  list_to_binary( lists:join(",", [ mk_sig_type(E) || {_,E} <- CS ]) ).

mk_sig_type({indexed,A}) ->
  mk_sig_type(A);
mk_sig_type({darray,A}) ->
  <<(mk_sig_type(A))/binary,"[]">>;
mk_sig_type({tuple,Type}) ->
  <<"(",(mk_sig_arr(Type))/binary,")">>;

mk_sig_type({bytes,N}) when N>0 andalso 32>=N ->
  <<"bytes",(integer_to_binary(N))/binary>>;
mk_sig_type(Type) when is_atom(Type) ->
  atom_to_binary(Type,utf8).


mk_fullsig({{constructor,default},CS,R}) ->
  list_to_binary([ "constructor(", mk_sig_farr(CS), ")",
                   case R of
                     [] -> [];
                     undefined -> [];
                     _ when is_list(R) ->
                       [" returns (",mk_sig_farr(R),")"]
                   end ]);

mk_fullsig({{EventOrFunction,Name},CS,R}) when EventOrFunction == event;
                                               EventOrFunction == function;
                                               EventOrFunction == error ->
  list_to_binary([ Name, "(", mk_sig_farr(CS), ")",
                   case R of
                     [] -> [];
                     undefined -> [];
                     _ when is_list(R) ->
                       [" returns (",mk_sig_farr(R),")"]
                   end ]).

mk_sig_farr(CS) ->
  list_to_binary( lists:join(", ", [ [mk_sig_ftype(E)|
                                     if N == <<>> -> []; true -> [" ",N] end] || {N,E} <- CS ]) ).
mk_sig_ftype({indexed,A}) ->
  <<(mk_sig_ftype(A))/binary," indexed">>;
mk_sig_ftype({darray,A}) ->
  <<(mk_sig_ftype(A))/binary,"[]">>;
mk_sig_ftype({{fixarray,N},A}) ->
  <<(mk_sig_ftype(A))/binary,"[",(integer_to_binary(N))/binary,"]">>;
mk_sig_ftype({tuple,Type}) ->
  <<"(",(mk_sig_farr(Type))/binary,")">>;

mk_sig_ftype({bytes,N}) when N>0 andalso 32>=N ->
  <<"bytes",(integer_to_binary(N))/binary>>;
mk_sig_ftype(Type) when is_atom(Type) ->
  atom_to_binary(Type,utf8).



sig_split(Signature) ->
  {ok,{{function, Name},Args, _}} = parse_signature(Signature),
  {Name, Args}.

parse_signature(String) when is_binary(String) ->
  parse_signature(binary_to_list(String));

parse_signature(String) when is_list(String) ->
  case re:run(String,"(^\.+\\\))\s\*returns\s\*(\\\(.+)") of
    {match,[_,{S0,L0},{S1,L1}]} ->
      parse_signature(string:substr(String,S0+1,L0),
                      string:substr(String,S1+1,L1)
                     );
    _ ->
      String1=case hd(String) of
                FC when FC>=$A andalso $Z>=FC ->
                  [$x,$x,$x|String];
                _ ->
                  String
              end,
      {_,B,_}=erl_scan:string(String1),
      case contract_evm_abi_parser:parse(B) of
        {ok,{Name,R}} when is_atom(Name) ->
          BName=case atom_to_binary(Name) of
                 <<"xxx",CapName/binary>> -> CapName;
                  Other -> Other
                end,
          {ok,{{function, BName},(R), undefined}};
        {error, Err} ->
          {error, Err}
      end
  end.

parse_signature(String0,RetString) when is_list(String0), is_list(RetString) ->
  String1=case hd(String0) of
    FC when FC>=$A andalso $Z>=FC ->
      [$x,$x,$x|String0];
    _ ->
      String0
  end,
  {_,B,_}=erl_scan:string(String1),
  case contract_evm_abi_parser:parse(B) of
    {ok,{Name,R}} when is_atom(Name) ->
      {_,C,_}=erl_scan:string(RetString),
      case contract_evm_abi_parser:parse(C) of
        {ok,{_,R2}} ->
          BName=case atom_to_binary(Name) of
                 <<"xxx",CapName/binary>> -> CapName;
                  Other -> Other
                end,
          {ok,{{function, BName},(R), R2}};
        {error, Err} ->
          {error, Err}
      end;
    {error, Err} ->
      {error, Err}
  end.



parse_abilist([]) ->
  [];
parse_abilist([_|_]=JSON) ->
  lists:filtermap(
    fun parse_item/1,
    JSON
   ).

load_abifile(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  jsx:decode(Bin,[return_maps]).

parse_abifile(Filename) ->
  JSON=load_abifile(Filename),
  parse_abilist(JSON).

parse_item(#{
             <<"outputs">> := O,
             <<"name">> := Name,
             <<"inputs">> := I,
             <<"type">> := <<"function">>
            }) ->
  {true,{{function,Name},convert_io(I),convert_io(O)}};
parse_item(#{
             <<"name">> := Name,
             <<"inputs">> := I,
             <<"type">> := <<"event">>
            }) ->
  {true,{{event,Name},convert_io(I),[]}};
parse_item(#{
             <<"name">> := Name,
             <<"inputs">> := I,
             <<"type">> := <<"error">>
            }) ->
  {true,{{error,Name},convert_io(I),[]}};
parse_item(#{
             <<"inputs">> := I,
             <<"type">> := <<"constructor">>
            }) ->
  {true,{{constructor,default},convert_io(I),[]}};
parse_item(#{}=_Any) ->
  false.

convert_io(List) ->
  lists:map(
    fun(#{
          <<"name">> := Name,
          <<"type">> := <<"tuple">>,
          <<"components">>:= C}) ->
        {Name, {tuple, convert_io(C)}};
       (#{
          <<"name">> := Name,
          <<"type">> := <<"tuple[]">>,
          <<"components">>:= C}) ->
        {Name, {darray,{tuple, convert_io(C)}}};
       (#{
          <<"name">> := Name,
          <<"type">> := Type,
          <<"indexed">>:= true}) ->
        {Name, {indexed,parse_type(Type)}};
       (#{
          <<"name">> := Name,
          <<"type">> := Type}) ->
        {Name, parse_type(Type)}
    end, List).


parse_type(L) ->
  case binary:split(L,<<"[">>) of
    [ L1 ] ->
      convert_type1(L1);
    [ L1, L2 ] ->
      case binary:split(L2,<<"]">>) of
        [<<>>,<<>>] ->
          {darray,convert_type1(L1)};
        [AS,<<>>] ->
          ArrSize=binary_to_integer(AS),
          {{fixarray,ArrSize},convert_type1(L1)}
      end
  end.

convert_type1(<<"string">>) -> string;
convert_type1(<<"address">>) -> address;
convert_type1(<<"uint256">>) -> uint256;
convert_type1(<<"uint32">>) -> uint32;
convert_type1(<<"uint8">>) -> uint8;
convert_type1(<<"bytes">>) -> bytes;
%convert_type1(<<"bytes[]">>) -> {darray,bytes};
convert_type1(<<"uint",N/binary>>=E) ->
  S=binary_to_integer(N),
  if(S<1) ->
      throw({'bad_type',E});
    (S>256) ->
      throw({'bad_type',E});
    (S rem 8) > 0 ->
      throw({'bad_type',E});
    true ->
      {uint,S}
  end;

convert_type1(<<"bytes",N/binary>>=E) ->
  S=binary_to_integer(N),
  if(S<1) -> throw({'bad_type',E});
    (S>32) -> throw({'bad_type',E});
    true -> ok
  end,
  {bytes,S};

%convert_type1(<<"string[]">>) -> {darray,string};

%convert_type1(<<"uint8[",N/binary>>) when size(N)>0 ->
%  case binary:split(N,<<"]">>) of
%    [<<>>,<<>>] -> {darray,uint8};
%    [S,<<>>] ->
%  Size=binary_to_integer(S),
%  {{fixarray,Size},uint8}
%  end;
%
%convert_type1(<<"uint256[",N/binary>>) when size(N)>0 ->
%  case binary:split(N,<<"]">>) of
%    [<<>>,<<>>] -> {darray,uint256};
%    [S,<<>>] ->
%  Size=binary_to_integer(S),
%  {{fixarray,Size},uint256}
%  end;
%convert_type1(<<"uint8[]">>) -> {darray,uint8};
convert_type1(<<"bool">>) -> bool.

encode_type(<<Input:256/big>>, uint256) ->
  <<Input:256/big>>;
encode_type(Input, uint256) when is_integer(Input) ->
  <<Input:256/big>>;

encode_type(Input, uint32) when is_integer(Input) ->
  <<(Input band 16#ffffffff):256/big>>;

encode_type(<<Input:256/big>>, uint8) ->
  <<(Input band 255):256/big>>;
encode_type(Input, uint8) when is_integer(Input) ->
  <<(Input band 255):256/big>>;

encode_type(Input, bool) when is_integer(Input) ->
  Val=if Input==0 -> 0; true -> 1 end,
  <<Val:256/big>>;
encode_type(Input, bool) when is_atom(Input) ->
  Val=if Input==false -> 0; true -> 1 end,
  <<Val:256/big>>;

encode_type(<<Input:32/binary>>, {bytes,32}) ->
  <<Input:32/binary>>;

encode_type(<<Input/binary>>, {bytes,N}) ->
  if(size(Input)>N) -> throw({does_not_fit,Input});
    true -> ok
  end,
  <<0:((32-N)*8)/big,Input:N/binary>>;

encode_type(Input, address) when is_integer(Input) ->
  <<Input:256/big>>;
encode_type(Input, address) ->
  IVal=binary:decode_unsigned(Input),
  <<IVal:256/big>>;

encode_type(_, Type) ->
  throw({'unexpected_type',Type}).

decode_abi_call(<<Selector:32/big,Data/binary>>, AbiFile, ProcFun) when is_list(AbiFile) ->
  try_decode_abi_call(Selector, Data, AbiFile, ProcFun).

decode_abi_call(<<Selector:32/big,Data/binary>>, AbiFile) when is_list(AbiFile) ->
  try_decode_abi_call(Selector, Data, AbiFile, undefined).

try_decode_abi_call(_Selector,_Data,[],_) ->
  {error, not_found};

try_decode_abi_call(Selector,Data,[{{function,_},In,_}=AbiSpec|AbiFile],ProcFun) ->
  Signature=contract_evm_abi:mk_sig(AbiSpec),
  I=contract_evm_abi:sig32(Signature),
  if(I==Selector) ->
      {ok, {Signature,decode_abi(Data,In,[],ProcFun)}};
    true ->
      try_decode_abi_call(Selector,Data,AbiFile,ProcFun)
  end;

try_decode_abi_call(Selector,Data,[_|AbiFile],ProcFun) ->
  try_decode_abi_call(Selector,Data,AbiFile,ProcFun).

encode_abi_call(D,ABIStr) ->
  case contract_evm_abi:parse_signature(ABIStr) of
    {ok,{{function,<<"undefined">>},
         ABI, _}} ->
      encode_abi(D,ABI);
    {ok,{{function,_},ABI,_}=Sig} ->
      Bin=encode_abi(D,ABI),
      I=contract_evm_abi:sig32(contract_evm_abi:mk_sig(Sig)),
      <<I:32/big,Bin/binary>>
  end.

encode_abi(D,ABI) ->
  HdLen=length(ABI)*32,
  encode_typed(D,ABI,<<>>,<<>>,HdLen).

encode_typed([],[], Hdr, Body, _BOff) ->
  <<Hdr/binary,Body/binary>>;

encode_typed([Val|RVal],[{_Name,{tuple,List}}|RType], Hdr, Body, BOff) ->
  HdLen=length(List)*32,
  EncStr=encode_typed(Val, List, <<>>, <<>>, HdLen),

  encode_typed(RVal, % CHECK IT
               RType,
               <<Hdr/binary,BOff:256/big>>,
               <<Body/binary,EncStr/binary>>,
               BOff+size(EncStr));

encode_typed([Val|RVal],[{_Name,{darray,Type}}|RType], Hdr, Body, BOff) ->
  Len=length(Val),
  EncStr=encode_typed(Val,[{'_naked',Type} || _ <- lists:seq(1,Len)], <<Len:256/big>>, <<>>,
                      (Len*32)),

  encode_typed(RVal,
               RType,
               <<Hdr/binary,BOff:256/big>>,
               <<Body/binary,EncStr/binary>>,
               BOff+size(EncStr));

encode_typed([Val|RVal],[{_Name,string}|RType], Hdr, Body, BOff) ->
  EncStr=encode_str(Val),
  encode_typed(RVal,
               RType,
               <<Hdr/binary,BOff:256/big>>,
               <<Body/binary,EncStr/binary>>,
               BOff+size(EncStr));

encode_typed([Val|RVal],[{_Name,bytes}|RType], Hdr, Body, BOff) ->
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

tuple_array_test() ->
  ABI=[{<<>>,{darray,{tuple,[{<<"id">>,uint256},{<<"text">>,string}]}}}],
  Bin=hex:decode(
        "0000000000000000000000000000000000000000000000000000000000000020"
        "0000000000000000000000000000000000000000000000000000000000000002"
        "0000000000000000000000000000000000000000000000000000000000000040"
        "00000000000000000000000000000000000000000000000000000000000000C0"
        "000000000000000000000000000000000000000000000000000000000000007B"
        "0000000000000000000000000000000000000000000000000000000000000040"
        "0000000000000000000000000000000000000000000000000000000000000003"
        "3332310000000000000000000000000000000000000000000000000000000000"
        "000000000000000000000000000000000000000000000000000000000000029A"
        "0000000000000000000000000000000000000000000000000000000000000040"
        "0000000000000000000000000000000000000000000000000000000000000003"
        "3636360000000000000000000000000000000000000000000000000000000000"),
  Bin2=encode_abi([[[123,"321"],[666,"666"]]],ABI),
  Dec=decode_abi(Bin,ABI),
  [
   ?assertMatch([{_, [[{<<"id">>,123},{<<"text">>,<<"321">>}],
                      [{<<"id">>,666},{<<"text">>,<<"666">>}]]}],
                Dec
               ),
   ?assertEqual(Bin, Bin2)
  ].


