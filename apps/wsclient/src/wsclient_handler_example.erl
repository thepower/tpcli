-module(wsclient_handler_example).
-export([
         start_link/0,
         before_connect/3,
         before_upgrade/3,
         connected/4,
         closed/4,
         handle_message/3,
         handle_info/3
        ]).

start_link() ->
  wsclient:start_link(
    #{name=>test1,
      address=>"127.0.0.1",
      port=>49841,
      wsheaders=>[{<<"sec-websocket-protocol">>, <<"thepower-nodesync-v1">>}],
      %opts=>#{ transport=>tls, protocols => [http]},
      opts=>#{ transport=>tcp, protocols => [http]},
      wsurl=>"/api/ws",
      handler=>?MODULE
     }).

before_connect(_Name, Try, State) ->
  logger:info("~s try ~p ~p",[?FUNCTION_NAME,Try,State]),
  {ok, State}.

before_upgrade(_Name, Try, State) ->
  logger:info("~s try ~p ~p",[?FUNCTION_NAME,Try,State]),
  {ok, State}.

connected(_Name, Status, Headers, State) ->
  logger:info("~s (~p:~p) ~p",[?FUNCTION_NAME, Status, Headers, State]),
  {ok, State}.

closed(_Name, Reason, Try, State) ->
  logger:info("~s (~p:~w) ~p",[?FUNCTION_NAME, Reason, Try, State]),
  {ok, State}.

handle_message(_Name, {binary,Message}, State) ->
  {ok,M}=msgpack:unpack(Message),
  logger:info("Got message ~p ~p",[M, State]),
  {noreply, State};
  %{reply, {text,[<<"Preved, medved, i got ">>, Message]}, State};

handle_message(_Name, Message, State) ->
  logger:info("~s ~p ~p",[?FUNCTION_NAME, Message, State]),
  {noreply, State}.

handle_info(_Name, {text, Data}, State) ->
  logger:info("~s ~p ~p",[?FUNCTION_NAME, Data, State]),
  {reply, {text,Data}, State};

handle_info(_Name, Info, State) ->
  logger:info("~s ~p ~p",[?FUNCTION_NAME, Info, State]),
  {noreply, State}.

