-module(tp_storage).
-export([find_my_task_id/5, new_task/8, new_prov/6]).

new_prov(Node, Addr, MyAddr, Priv, UploadURL, BaseURL) ->
  {ok,Seq}=tpapi2:get_seq(Node, MyAddr),
  ABI=[
       <<16#74ca8971:32/big>>, %add_prov
       tp_abi:encode_simple([
                             list_to_binary([UploadURL]),
                             list_to_binary([BaseURL])
                            ])],

  Tx=tx:sign(
       tx:construct_tx(
         #{kind=>generic,
           t => os:system_time(millisecond),
           seq => Seq+1,
           from => MyAddr,
           to => Addr,
           payload => [
                       #{purpose=>gas, amount=>15000, cur=><<"FTT">>}
                      ],
           call => #{
                     args=>[iolist_to_binary(ABI)],
                     function=>"0x0"
                    },
           ver=>2
          }
        ),
       Priv),
  %case tpapi2:submit_tx(Node ,Tx) of
  %  {ok,#{<<"ok">>:=true,
  %        <<"block">> := _BlkID}} ->
      case find_my_prov(
             Node,
             Addr,
             MyAddr) of
        {ok, ID} ->
          {ok, ID};
        {error, Error} ->
          io:format("Something went wrong, error ~p~n",[Error]),
          {error, Error}
   %   end;
   % {ok, #{}=R} ->
   %   logger:notice("Error ~p",[R]),
   %   {error, block_not_found};
   % {error, E} ->
   %   {error, E}
  end.



new_task(Node, Addr, MyAddr, Priv, TBucket, Hash, Expire, Size) ->
  {ok,Seq}=tpapi2:get_seq(Node, MyAddr),
  ABI=[
       <<16#cc839f9d:32/big>>,
       %<<16#74ca8971:32/big>>, add_prov
       tp_abi:encode_simple([
                             list_to_binary([TBucket]),
                             binary:decode_unsigned(Hash),
                             Expire,
                             Size
                            ])],

  Tx=tx:sign(
       tx:construct_tx(
         #{kind=>generic,
           t => os:system_time(millisecond),
           seq => Seq+1,
           from => MyAddr,
           to => Addr,
           payload => [
                       #{purpose=>gas, amount=>15000, cur=><<"FTT">>}
                      ],
           call => #{
                     args=>[iolist_to_binary(ABI)],
                     function=>"0x0"
                    },
           ver=>2
          }
        ),
       Priv),
  case tpapi2:submit_tx(Node ,Tx) of
    {ok,#{<<"ok">>:=true,
          <<"block">> := _BlkID}} ->
      case tp_storage:find_my_task_id(
             Node,
             Addr,
             MyAddr,
             list_to_binary(TBucket),
             Hash) of
        {ok, ID} ->
          {ok, ID};
        {error, Error} ->
          io:format("Something went wrong, error ~p~n",[Error]),
          {error, Error}
      end;
    {ok, #{}=R} ->
      logger:notice("Error ~p",[R]),
      {error, block_not_found};
    {error, E} ->
      {error, E}
  end.


find_my_prov(Node, Addr, MyAddr) ->
  Max=case tp_evm:q(Node , Addr, <<"storageNodesCount()">>, []) of
        {return, <<Number:256/big>>, _} ->
          Number
      end,
  check_prov(Node, Max, Addr, binary:decode_unsigned(MyAddr), 5).

check_prov(_, _, _, _, 0) ->
  {error, not_found};
check_prov(_, 0, _, _, _) ->
  {error, not_found};

check_prov(Node, TaskID, Addr, MyAddr, Left) ->
  case tp_evm:q(Node , Addr, <<"getProvider(uint256)">>, [TaskID]) of
    {return, Bin, _} ->
      Outs=[{owner,address}, {url1,string}, {url2,string}],
      #{owner:=Owner}=_Dec=tp_abi:decode_abi_map(Bin, Outs),
      if Owner==MyAddr ->
           {ok, TaskID};
         true ->
           check_prov(Node, TaskID-1, Addr, MyAddr, Left-1)
      end;
    Other ->
      logger:error("Can't decode getProvider(uint256) call result ~p",[Other])
  end.





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

