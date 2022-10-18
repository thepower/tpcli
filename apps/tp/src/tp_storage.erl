-module(tp_storage).
-export([find_my_task_id/5, get_task/4, get_prov/4]).
-export([new_task/8]).
-export([new_prov/6]).

get_task(Node, Addr, TaskID, _Opts) ->
  case tp_evm:q(Node , Addr, <<"getTask(uint256)">>, [TaskID]) of
    {return, Bin, _} ->
      Outs=[{owner,address}, {name,string}, {hash,uint256}, {size,uint256}, {time,uint256},
            {expire,uint256}, {uploader,uint256}, {status,uint256}],
      #{}=R=tp_abi:decode_abi_map(Bin, Outs),
      {ok, R};
    Other ->
      logger:error("Can't decode getTask(uint256) call result ~p",[Other]),
      {error, Other}
  end.

get_prov(Node, Addr, ProvID, _Opts) ->
  case tp_evm:q(Node , Addr, <<"getProvider(uint256)">>, [ProvID]) of
    {return, Bin, _} ->
      Outs=[{owner,address}, {base_url,string}, {upload_url,string}],
      #{owner:=_Owner}=Dec=tp_abi:decode_abi_map(Bin, Outs),
      {ok, Dec};
    Other ->
      logger:error("Can't decode getProvider(uint256) call result ~p",[Other]),
      {error, Other}
  end.

new_prov(Addr, MyAddr, Seq, UploadURL, BaseURL, PayloadFun) ->
  ABI=[
       <<16#74ca8971:32/big>>, %add_prov
       tp_abi:encode_simple([
                             list_to_binary([UploadURL]),
                             list_to_binary([BaseURL])
                            ])],

  TxTpl=#{kind=>generic,
       t => os:system_time(millisecond),
       seq => Seq+1,
       from => MyAddr,
       to => Addr,
       payload => [#{purpose=>gas, amount=>17000, cur=><<"CUR1">>},
                   #{purpose=>srcfee, amount=>17000, cur=><<"CUR1">>}
                  ],
       call => #{
                 args=>[iolist_to_binary(ABI)],
                 function=>"0x0"
                },
       ver=>2
      },

  Size=size(maps:get(body,tx:construct_tx(TxTpl))),

  P=PayloadFun(2000000,Size),
  Tx=tx:construct_tx(TxTpl#{payload => P}),
  Tx.


new_task(Addr, MyAddr, Seq, TBucket, Hash, Expire, Size, PayloadFun) ->
  ABI=[
       <<16#cc839f9d:32/big>>,
       %<<16#74ca8971:32/big>>, add_prov
       %registerProvider(
       tp_abi:encode_simple([
                             list_to_binary([TBucket]),
                             binary:decode_unsigned(Hash),
                             Expire,
                             Size
                            ])],
  TxTpl=#{kind=>generic,
          t => os:system_time(millisecond),
          seq => Seq+1,
          from => MyAddr,
          to => Addr,
          payload => [#{purpose=>gas, amount=>17000, cur=><<"CUR1">>},
                      #{purpose=>srcfee, amount=>17000, cur=><<"CUR1">>},
                      #{purpose=>transfer, amount=>17000, cur=><<"CUR1">>}
                     ],
          call => #{
                    args=>[iolist_to_binary(ABI)],
                    function=>"0x0"
                   },
          ver=>2
         },

  PreTX=tx:construct_tx(TxTpl),
  TxSize=size(maps:get(body,PreTX)),
  P=PayloadFun(2000000,TxSize),

  Tx=tx:construct_tx(TxTpl#{ payload => P }),
  Tx.

%find_my_prov(Node, Addr, MyAddr) ->
%  Max=case tp_evm:q(Node , Addr, <<"storageNodesCount()">>, []) of
%        {return, <<Number:256/big>>, _} ->
%          Number
%      end,
%  check_prov(Node, Max, Addr, binary:decode_unsigned(MyAddr), 5).
%
%check_prov(_, _, _, _, 0) ->
%  {error, not_found};
%check_prov(_, 0, _, _, _) ->
%  {error, not_found};
%
%check_prov(Node, TaskID, Addr, MyAddr, Left) ->
%  case tp_evm:q(Node , Addr, <<"getProvider(uint256)">>, [TaskID]) of
%    {return, Bin, _} ->
%      Outs=[{owner,address}, {base_url,string}, {upload_url,string}],
%      #{owner:=Owner}=_Dec=tp_abi:decode_abi_map(Bin, Outs),
%      if Owner==MyAddr ->
%           {ok, TaskID};
%         true ->
%           check_prov(Node, TaskID-1, Addr, MyAddr, Left-1)
%      end;
%    Other ->
%      logger:error("Can't decode getProvider(uint256) call result ~p",[Other])
%  end.
%
%
%
%
%
find_my_task_id(Node, Addr, MyAddr, MyBucket, MyHash) ->
  Max=case tp_evm:q(Node , Addr, <<"storageTasksCount()">>, []) of
        {return, <<NumberOfTasks:256/big>>, _} ->
          io:format("Total tasks in contract ~w~n",[NumberOfTasks]),
          NumberOfTasks
      end,
  check_task(Node, Max, Addr, binary:decode_unsigned(MyAddr), MyBucket, binary:decode_unsigned(MyHash), 5).

check_task(_, _, _, _, _, _, 0) ->
  {error, not_found};
check_task(_, 0, _, _, _, _, _) ->
  {error, not_found};

check_task(Node, TaskID, Addr, MyAddr, MyBucket, MyHash, Left) ->
  case tp_evm:q(Node , Addr, <<"getTask(uint256)">>, [TaskID]) of
    {return, Bin, _} ->
      Outs=[{owner,address}, {name,string}, {hash,uint256}, {size,uint256}, {time,uint256},
            {expire,uint256}, {uploader,uint256}, {status,uint256}],
      #{hash:=Hash,
        owner:=Owner,
        name:=Name}=tp_abi:decode_abi_map(Bin, Outs),
      if Hash==MyHash, Owner==MyAddr, Name==MyBucket ->
           {ok, TaskID};
         true ->
           check_task(Node, TaskID-1, Addr, MyAddr, MyBucket, MyHash, Left-1)
      end;
    Other ->
      logger:error("Can't decode getTask(uint256) call result ~p",[Other])
  end.

