-module(tp_show).

-export([tx/1, displaybin/1]).

displaybin(<<>>) ->
  [];
displaybin(<<B1:32/binary,Rest/binary>>) ->
  [
  io_lib:format("~s~n",[hex:encode(B1)])
  | displaybin(Rest)
  ];
displaybin(Rest) ->
  [io_lib:format("~s~n",[hex:encode(Rest)])].

tx(Tx) ->
  Tx4Disp=maps:map(
            fun
              (sig, Signatures) ->
                [
                 [$0,$x|binary_to_list(hex:encode(Sig))] || Sig <- Signatures
                ];
              (from, Addr) ->
                binary_to_list(naddress:encode(Addr));
              (to, Addr) ->
                binary_to_list(naddress:encode(Addr));
              (_K,V) ->
                V
            end, Tx),
  D=maps:fold(
      fun
        (call,#{args := [<<Signature:32/big,Args/binary>>], function := "0x0"=Func}, A) when is_binary(Args) ->
          [A, io_lib:format(
                if(A==[]) ->
                    "  call => #{ function => ~p, args => \"0x~s\n~s\" }";
                  true ->
                    ",~n  call => #{ function => ~p, args => \"0x~s\n~s\" }"
                end,
                [ Func, 
                  hex:encode(<<Signature:32/big>>),
                  displaybin(Args) ])];
        (call,#{args := [Args], function := Func}, A) when is_binary(Args) ->
          [A, io_lib:format(
                if(A==[]) ->
                    "  call => #{ function => ~p, args => \"0x\n~s\" }";
                  true ->
                    ",~n  call => #{ function => ~p, args => \"0x\n~s\" }"
                end,
                [ Func, displaybin(Args) ])];
        (body, Bin, A) ->
          [A,io_lib:format("  body => \"0x~s\"",[displaybin(Bin)])];
        (K,V,A) ->
          if(A==[]) ->
              [A,io_lib:format("  ~p => ~p",[K,V])];
            true ->
              [A,io_lib:format(",~n  ~p => ~p",[K,V])]
          end
      end, [], Tx4Disp),
  io:format("Tx #{~n~s~n}~n",[D]).
