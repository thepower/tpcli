%%%-------------------------------------------------------------------
%%% @author Vladimir Goncharov
%%% @copyright (C) 2022
%%% @doc
%%%
%%% @end
%%% Created : 2022-08-29
%%%-------------------------------------------------------------------
-module(wsclient).

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([
         callback_mode/0,
         init/1,
         format_status/2,

         do_connect/3,
         do_upgrade/3,
         do_work/3,
         do_close/3,

         terminate/3,
         code_change/4
        ]).

-record(state, {
          name,
          address,
          port,
          opts,
          wsurl,
          wsheaders,
          handler_module,
          handler_state,
          reconnect_timeout,
          idle_timeout,
          open_timeout,
          connect_try,
          pid
         }).

-define(IDLE_TIMEOUT, Data#state.idle_timeout).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

callback_mode() ->
    [state_functions, state_enter].

init(Args) ->
    %process_flag(trap_exit, true),
    {ok, do_connect, parse_args(Args)}.

format_status(_Opt, [_PDict, State, Data]) ->
    [{data, [{"State", {State, Data}}]}].

do_connect(enter, _, Data) ->
  case (Data#state.handler_module):before_connect(Data#state.name,
                                                  Data#state.connect_try+1,
                                                  Data#state.handler_state) of
    {ok, State1} ->
      {ok, Pid} = gun:open(Data#state.address,
                           Data#state.port,
                           Data#state.opts),
      {keep_state,
       Data#state{pid=Pid, connect_try=Data#state.connect_try+1,handler_state=State1},
       [{state_timeout,Data#state.open_timeout,gun_timeout}]
      };
    stop ->
      stop
  end;


do_connect(state_timeout, gun_timeout, Data) ->
  logger:debug("~s do_connect timeout",[Data#state.name]),
  {next_state, do_close, Data};

do_connect(info, {gun_down, Pid, _Protocol, closed, _, _}=Msg, Data) when Pid==Data#state.pid ->
  logger:debug("~s do_connect info ~p",[Data#state.name, Msg]),
  {next_state, do_close, Data};

do_connect(info, {gun_up, Pid, _Protocol}=Msg, Data) when Pid==Data#state.pid ->
  logger:debug("~s do_connect info ~p",[Data#state.name, Msg]),
  {next_state, do_upgrade, Data};

do_connect(info, Msg, Data) ->
  logger:notice("~s do_connect unexpected info ~p, dropped",[Data#state.name, Msg]),
  keep_state_and_data;

do_connect(Kind, Msg, Data) ->
  logger:notice("~s do_connect ~p:~p, dying",[Data#state.name, Kind, Msg]),
  stop.

do_upgrade(enter, do_connect, Data) ->
  case (Data#state.handler_module):before_upgrade(Data#state.name,
                                                  Data#state.connect_try,
                                                  Data#state.handler_state) of
    {ok, State1} ->
      gun:ws_upgrade(Data#state.pid, Data#state.wsurl, Data#state.wsheaders),
      {keep_state, Data#state{handler_state=State1}};
    stop ->
      stop
  end;

do_upgrade(info, {gun_upgrade,Pid,_Ref,Status,Headers}, Data) when Pid==Data#state.pid ->
  case (Data#state.handler_module):connected(Data#state.name,
                                             Status,
                                             Headers,
                                             Data#state.handler_state) of
    {ok, State1} ->
      {next_state, do_work, Data#state{handler_state=State1, connect_try=0}};
    stop ->
      stop
  end;

do_upgrade(info, {gun_down, Pid, _Protocol, closed, _, _}, Data) when Pid==Data#state.pid ->
  logger:error("~s Upgrade error: closed",[Data#state.name]),
  {next_state, do_close, Data};

do_upgrade(info, {gun_response, Pid, _Ref, _Fin, ErrorCode, _Headers}, Data) when Pid==Data#state.pid ->
  logger:error("~s Upgrade error: ~p", [Data#state.name, ErrorCode]),
  {next_state, do_close, Data};

do_upgrade(info, Msg, Data) ->
  logger:notice("~s do_upgrade unexpected info ~p, dropped",[Data#state.name, Msg]),
  keep_state_and_data;

do_upgrade(Kind, Msg, Data) ->
  logger:notice("~s do_upgrade ~p:~p, dying",[Data#state.name, Kind, Msg]),
  stop.

do_close(enter, _, Data) ->
  gun:close(Data#state.pid),
  Timeout=max(100,min(Data#state.reconnect_timeout*Data#state.connect_try,300_000)),
  %logger:info("~s Closed, try ~w, wait ~w",[Data#state.name, Data#state.connect_try, Timeout]),
  erlang:send_after(Timeout, self(), connect),
  {keep_state, Data#state{pid=undefined}};

do_close(info, {gun_data, _Pid, _SockRef, _Fin, _Body}, _Data) ->
  %flush error response body
  keep_state_and_data;

do_close(info, connect, Data) ->
  {next_state, do_connect, Data};

do_close(info, Msg, Data) ->
  logger:notice("~s do_close unexpected info ~p, dropped",[Data#state.name, Msg]),
  keep_state_and_data;

do_close(Kind, Msg, Data) ->
  logger:notice("~s do_close unexpected message ~p:~p, dying",[Data#state.name, Kind, Msg]),
  stop.

do_work(enter, do_upgrade, _Data) ->
  keep_state_and_data;

do_work(info, {gun_down, Pid, _Proto, closed, _, _}, Data) when Pid==Data#state.pid ->
  {next_state, do_close, Data};

do_work(info, {gun_ws, Pid, _Ref, Payload}, Data) when Pid==Data#state.pid ->
  case (Data#state.handler_module):handle_message(Data#state.name,
                                                  Payload,
                                                  Data#state.handler_state) of
    {noreply, State1} ->
      {keep_state, Data#state{handler_state=State1},
       [{state_timeout,?IDLE_TIMEOUT,idle}]
      };
    {reply, Replys, State1} when is_list(Replys) ->
      [
       ok=gun:ws_send(Data#state.pid, Reply)
       || Reply <- Replys
      ],
      {keep_state, Data#state{handler_state=State1},
       [{state_timeout,?IDLE_TIMEOUT,idle}]
      };
    {reply, Reply, State1} ->
      ok=gun:ws_send(Data#state.pid, Reply),
      {keep_state, Data#state{handler_state=State1},
       [{state_timeout,?IDLE_TIMEOUT,idle}]
      }
  end;

do_work(info, Info, Data) ->
  case (Data#state.handler_module):handle_info(Data#state.name,
                                               Info,
                                               Data#state.handler_state) of
    {noreply, State1} ->
      {keep_state, Data#state{handler_state=State1}};
    {reply, Replys, State1} when is_list(Replys) ->
      [
       ok=gun:ws_send(Data#state.pid, Reply)
       || Reply <- Replys
      ],
      {keep_state, Data#state{handler_state=State1},
       [{state_timeout,?IDLE_TIMEOUT,idle}]
      };
    {reply, Reply, State1} ->
      ok=gun:ws_send(Data#state.pid, Reply),
      {keep_state, Data#state{handler_state=State1}}
  end;

do_work(state_timeout, idle, Data) ->
  ok=gun:ws_send(Data#state.pid, ping),
  {keep_state, Data,
   [{state_timeout,?IDLE_TIMEOUT,idle}]
  };

do_work(Kind, Msg, Data) ->
  logger:notice("~s do_work unexpected message ~p:~p, dying",[Data#state.name, Kind, Msg]),
  stop.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_args(#{name:=Name,address:=Addr,port:=Port,wsurl:=WsURL,handler:=M}=Args) ->
  true=is_atom(M),
  #state{
     name=Name,
     address=Addr,
     port=Port,
     opts=maps:get(opts,Args,#{}),
     wsurl=WsURL,
     wsheaders=maps:get(wsheaders,Args,[]),
     handler_module=M,
     handler_state=maps:get(state,Args,#{}),
     reconnect_timeout=maps:get(reconnect_timeout, Args, 500),
     idle_timeout=maps:get(idle_timeout, Args, 300_000),
     open_timeout=maps:get(open_timeout, Args, 5_000),
     connect_try=0
    }.

