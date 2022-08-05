-module(errm_driver).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("errm_driver.hrl").
-include("errm_connection.hrl").
-include("errm_error.hrl").
-include("errm_utils.hrl").
-include("errm_result.hrl").
-include("errm_gen_driver.hrl").

-behavior(gen_server).

%% public api
-export([
  start_link/1,
  start_link/2,
  stop/1,
  get_connection/1,
  get_connection/2,
  get_connection/3,
  release_connection/2,
  release_connection/3,
  change_from/3
]).
%% gen_server behavior
-export([
  init/1,
  terminate/2,
  handle_call/3,
  handle_cast/2,
  handle_info/2
]).

%% errm_driver public api

% @doc Start and link connection pool manager server.
% @param Init   Initialization record containing errm_gen_driver module and initialization parameters, pool options, and replica pool options.
% @returns `{ok, pid()} | {error, term()}'
% @equiv start_link(errm_driver, Init)
-spec start_link(Init :: errm_driver_init()) -> {ok, pid()} | {error, term()}.
start_link(Init = #errm_driver_init{}) ->
  start_link(?MODULE, Init).

% @doc Start and link connection pool manager server with name `Name'.
% @param Name   Name to register connection pool manager server with.
% @param Init   Initialization record containing errm_gen_driver module and initialization parameters, pool options, and replica pool options.
% @returns `{ok, pid()} | {error, term()}'
-spec start_link(Name :: atom(), Init :: errm_driver_init()) -> {ok, pid()} | {error, term()}.
start_link(Name, Init = #errm_driver_init{}) ->
  gen_server:start_link({local, Name}, ?MODULE, Init, []).

% @doc Stop connection pool manager server.
% @param Pid    Pid of connection pool manager server.
% @returns `ok'
-spec stop(Pid :: errm_server_ref()) -> ok.
stop(Pid) ->
  gen_server:stop(Pid).

% @doc Get connection from connection pool manager server.
% @param Pid    Pid of connection pool manager server.
% @returns `{ok, errm_connection()} | {error, errm_error()}'
% @equiv get_connection(Pid, main)
-spec get_connection(Pid :: errm_server_ref()) -> {ok, errm_connection()} | {error, errm_error()}.
get_connection(Pid) ->
  get_connection(Pid, main).

% @doc Get connection of type `ConnType' from connection pool manager server.
% @param Pid        Pid of connection pool manager server.
% @param ConnType   Connection type to acquire. One of `main' or `replica'.
% @returns `{ok, errm_connection()} | {error, errm_error()}'
% @equiv get_connection(Pid, ConnType, 5000)
-spec get_connection(
  Pid :: errm_server_ref(),
  ConnType :: errm_connection_type()
) -> {ok, errm_connection()} | {error, errm_error()}.
get_connection(Pid, ConnType) ->
  get_connection(Pid, ConnType, ?ERRM_DRIVER_DEFAULT_TIMEOUT).

% @doc Get connection of type `ConnType' from connection pool manager server, timing out after `Timeout' milliseconds.
% @param Pid        Pid of connection pool manager server.
% @param ConnType   Connection type to acquire. One of `main' or `replica'.
% @param Timeout    Number of milliseconds to wait for connection acquisition before timing out.
% @returns `{ok, errm_connection()} | {error, errm_error()}'
-spec get_connection(
  Pid :: errm_server_ref(),
  ConnType :: errm_connection_type(),
  Timeout :: pos_integer()
) -> {ok, errm_connection()} | {error, errm_error()}.
get_connection(Pid, ConnType, Timeout)
  when ConnType =:= main; ConnType =:= replica ->
  gen_server:call(Pid, {get_connection, ConnType, Timeout}, Timeout).

% @doc Release connection `Conn' to connection pool manager server.
% @param Pid    Pid of connection pool manager server.
% @param Conn   Pid of connection server acquired and to be released.
% @returns `ok | {error, errm_error()}'
% @equiv release_connection(Pid, Conn, 5000)
-spec release_connection(Pid :: errm_server_ref(), Conn :: errm_connection()) -> ok | {error, errm_error()}.
release_connection(Pid, Conn) ->
  release_connection(Pid, Conn, ?ERRM_DRIVER_DEFAULT_TIMEOUT).

% @doc Release connection `Conn' to connection pool manager server, timing out after `Timeout' milliseconds.
% @param Pid        Pid of connection pool manager server.
% @param Conn       Pid of connection server acquired and to be released.
% @param Timeout    Number of milliseconds to wait for connection release before timing out.
% @returns `ok | {error, errm_error()}'
-spec release_connection(
  Pid :: errm_server_ref(),
  Conn :: errm_connection(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.
release_connection(Pid, Conn, Timeout) ->
  gen_server:call(Pid, {release_connection, Conn}, Timeout).

% @doc Change the controlling reply tuple that's monitored by the connection pool for automatic connection release.
% @param Pid    Pid of connection pool manager server.
% @param Conn   Pid of connection server acquired.
% @param From   New reply tuple to be monitored by connection pool manager server for automatic connection release.
% @returns `ok | {error, errm_error()}'
-spec change_from(
  Pid :: errm_server_ref(),
  Conn :: errm_connection(),
  From :: gen_server:from()
) -> ok | {error, errm_error()}.
change_from(Pid, Conn, From) ->
  gen_server:call(Pid, {change_from, Conn, From}, ?ERRM_DRIVER_DEFAULT_TIMEOUT).

%% gen_server callbacks

-spec init(Init :: errm_driver_init()) -> {ok, errm_driver_state()}.
init(#errm_driver_init{
  module=Mod,
  args=Args,
  pool_opts=#errm_pool_opts{ min=Min, max=Max },
  replica_opts=ReplicaOpts
}) ->
  PoolArgs = create_pool_args(Min, Max),
  WorkerArgs = create_worker_args(Mod, Args),
  {ok, Pid} = poolboy:start_link(PoolArgs, WorkerArgs),
  ReplicaPid = case ReplicaOpts of
                 undefined -> undefined;
                 #errm_replica_init{ module=ReplicaMod, args=ReplicaArgs, pool_opts=ReplicaPoolOpts } ->
                   #errm_pool_opts{ min=ReplicaMin, max=ReplicaMax } = ReplicaPoolOpts,
                   ReplicaPoolArgs = create_pool_args(ReplicaMin, ReplicaMax),
                   ReplicaWorkerArgs = create_worker_args(ReplicaMod, ReplicaArgs),
                   {ok, RPid} = poolboy:start_link(ReplicaPoolArgs, ReplicaWorkerArgs),
                   RPid
               end,
  C = create_connection_monitor_table(),
  {ok, #errm_driver_state{ pool=Pid, replica_pool=ReplicaPid, waiting=queue:new(), conn_monitors=C }}.

-spec terminate(Reason :: term(), State :: errm_driver_state()) -> ok.
terminate(_Reason, #errm_driver_state{ waiting=W }) ->
  ok = terminate_waiting(queue:to_list(W)),
  ok.

-spec handle_call(
  Req :: {get_connection, ConnType :: errm_connection_type(), Timeout :: pos_integer()} |
         {release_connection, Conn :: errm_connection()} |
         {change_from, Conn :: errm_connection(), NFrom :: gen_server:from()},
  From :: gen_server:from(),
  State :: errm_driver_state()
) -> {noreply, errm_driver_state()} | {reply, ok, errm_driver_state()}.
handle_call({get_connection, ConnType, Timeout}, From, State) ->
  {ok, State2} = handle_acquire(From, ConnType, Timeout, back, State),
  {noreply, State2};
handle_call({release_connection, Conn}, _From, State=#errm_driver_state{ conn_monitors=C }) ->
  case ets:lookup(C, Conn) of
    [ConnMon=#errm_conn_mon{ id=Conn, type=connection, connection=Conn }] ->
      State2 = handle_release(ConnMon, State),
      {reply, ok, State2};
    _ ->
      {noreply, State}
  end;
handle_call({change_from, Conn, NFrom}, _From, State=#errm_driver_state{ conn_monitors=C }) ->
  case ets:lookup(C, Conn) of
    [ConnMon=#errm_conn_mon{ id=Conn, type=connection, connection=Conn }] ->
      State2 = handle_change_from(NFrom, ConnMon, State),
      {reply, ok, State2};
    _ ->
      {noreply, State}
  end;
handle_call(_Req, _From, State) ->
  {noreply, State}.

-spec handle_cast(Cast :: term(), State :: errm_driver_state()) -> {noreply,errm_driver_state()}.
handle_cast(_Cast, State) ->
  {noreply, State}.

-spec handle_info(
  {'DOWN', Ref :: reference(), process, Pid :: pid(), Info :: term()},
  State :: errm_driver_state()
) -> {noreply, errm_driver_state()}.
handle_info({'DOWN', MRef, process, Pid, _Info}, State=#errm_driver_state{ conn_monitors=C }) ->
  case ets:lookup(C, Pid) of
    [ConnMon=#errm_conn_mon{ id=Pid, type=from, from={Pid, _}, from_monitor=MRef  }] ->
      State2 = handle_release(ConnMon, State),
      {noreply, State2};
    [ConnMon=#errm_conn_mon{ id=Pid, type=connection, connection=Pid, connection_monitor=MRef }] ->
      State2 = handle_down_release(ConnMon, State),
      {noreply, State2};
    _ ->
      {noreply, State}
  end;
handle_info(_, State) ->
  {noreply, State}.

%% private functions

-spec create_pool_args(Min :: pos_integer(), Max :: pos_integer()) -> proplists:proplist().
create_pool_args(MinSize, MaxSize) when MinSize =< MaxSize ->
  [{worker_module, errm_connection}, {size, MinSize}, {max_overflow, MaxSize - MinSize}, {strategy, fifo}].

-spec create_worker_args(Mod :: module(), Args :: [term()]) -> errm_connection_init().
create_worker_args(Mod, Args) ->
  #errm_connection_init{ module=Mod, args=Args }.

-spec create_connection_monitor_table() -> ets:table().
create_connection_monitor_table() ->
  ets:new(?ERRM_DRIVER_CONNECTION_MONITORS, [named_table, set, protected, {keypos, 2}]).

-spec terminate_waiting(Waiting :: [errm_driver_wait()]) -> ok.
terminate_waiting([]) ->
  ok;
terminate_waiting([#errm_driver_wait{ from=From } | Rest]) ->
  Err = "internal driver crash terminating get connection context",
  gen_server:reply(From, errm_error:new(?ERRM_GET_CONNECTION_ERROR, Err)),
  terminate_waiting(Rest).

-spec handle_acquire(
  From :: gen_server:from(),
  ConnType :: errm_connection_type(),
  Timeout :: pos_integer(),
  Pos :: front | back,
  State :: errm_driver_state()
) -> {ok, errm_driver_state()}.
handle_acquire(From, main, Timeout, Pos, State=#errm_driver_state{ pool=P }) ->
  handle_acquire_from_pool(From, main, Timeout, Pos, P, State);
handle_acquire(From, replica, Timeout, Pos, State=#errm_driver_state{ replica_pool=P }) when P =/= undefined ->
  handle_acquire_from_pool(From, replica, Timeout, Pos, P, State);
handle_acquire(From, replica, _TImeout, _Pos, State) ->
  Err = errm_error:new(?ERRM_GET_CONNECTION_ERROR, "no replica pool initialized, cannot acquire replica connection"),
  gen_server:reply(From, {error, Err}),
  {ok, State}.

-spec handle_acquire_from_pool(
  From :: gen_server:from(),
  ConnType :: errm_connection_type(),
  Timeout :: pos_integer(),
  Pos :: front | back,
  Pool :: pid(),
  State :: errm_driver_state()
) -> {ok, errm_driver_state()}.
handle_acquire_from_pool(From, ConnType, Timeout, Pos, Pool, State) ->
  Conn = poolboy:checkout(Pool, false, Timeout),
  case Conn of
    full ->
      WaitState = handle_acquire_wait(From, ConnType, Timeout, Pos, State),
      {ok, WaitState};
    ConnPid ->
      case errm_connection:acquire(ConnPid) of
        ok ->
          MonState = handle_monitor_conn(ConnPid, ConnType, From, State),
          gen_server:reply(From, {ok, ConnPid}),
          {ok, MonState};
        {error, Err} ->
          gen_server:reply(From, {error, Err}),
          {ok, State}
      end
  end.

-spec handle_acquire_wait(
  From :: gen_server:from(),
  ConnType :: errm_connection_type(),
  Timeout :: pos_integer(),
  Pos :: front | back,
  State :: errm_driver_state()
) -> errm_driver_state().
handle_acquire_wait(From, ConnType, Timeout, front, State=#errm_driver_state{ waiting=Q }) ->
  Q2 = queue:in_r(#errm_driver_wait{ from=From, conn_type=ConnType, timeout=Timeout }, Q),
  State#errm_driver_state{ waiting=Q2 };
handle_acquire_wait(From, ConnType, Timeout, back, State=#errm_driver_state{ waiting=Q }) ->
  Q2 = queue:in(#errm_driver_wait{ from=From, conn_type=ConnType, timeout=Timeout }, Q),
  State#errm_driver_state{ waiting=Q2 }.

-spec handle_monitor_conn(
  Conn :: errm_connection(),
  ConnType :: errm_connection_type(),
  From :: gen_server:from(),
  State :: errm_driver_state()
) -> errm_driver_state().
handle_monitor_conn(ConnPid, ConnType, From={FromPid, _}, State=#errm_driver_state{ conn_monitors=C }) ->
  FromMonitorRef = erlang:monitor(process, FromPid),
  ConnMonitorRef = erlang:monitor(process, ConnPid),
  true = ets:insert(C, #errm_conn_mon{
    id=ConnPid,
    type=connection,
    connection=ConnPid,
    connection_monitor=ConnMonitorRef,
    from=From,
    from_monitor=FromMonitorRef,
    conn_type=ConnType
  }),
  true = ets:insert(C, #errm_conn_mon{
    id=FromPid,
    type=from,
    connection=ConnPid,
    connection_monitor=ConnMonitorRef,
    from=From,
    from_monitor=FromMonitorRef,
    conn_type=ConnType
  }),
  State.

-spec handle_release(
  ConnMon :: errm_conn_mon(),
  State :: errm_driver_state()
) -> errm_driver_state().
handle_release(
  ConnMon=#errm_conn_mon{ conn_type=main },
  State=#errm_driver_state{ pool=P }
) ->
  handle_release_from_pool(ConnMon, P, State);
handle_release(
  ConnMon=#errm_conn_mon{ conn_type=replica },
  State=#errm_driver_state{ replica_pool=P }
) ->
  handle_release_from_pool(ConnMon, P, State).

-spec handle_release_from_pool(
  ConnMon :: errm_conn_mon(),
  Pool :: pid(),
  State :: errm_driver_state()
) -> errm_driver_state().
handle_release_from_pool(
  #errm_conn_mon{ connection=Conn, connection_monitor=CMon, from={FromPid, _}, from_monitor=FromMon },
  Pool,
  State=#errm_driver_state{ conn_monitors=C }
) ->
  true = ets:delete(C, Conn),
  true = ets:delete(C, FromPid),
  true = erlang:demonitor(CMon),
  true = erlang:demonitor(FromMon),
  errm_connection:release(Conn),
  poolboy:checkin(Pool, Conn),
  handle_next_wait(State).

-spec handle_down_release(
  ConnMon :: errm_conn_mon(),
  State :: errm_driver_state()
) -> errm_driver_state().
handle_down_release(
  #errm_conn_mon{ connection=Conn, type=connection, connection_monitor=CMon, from={FromPid, _}, from_monitor=FromMon },
  State=#errm_driver_state{ conn_monitors=C }
) ->
  true = ets:delete(C, Conn),
  true = ets:delete(C, FromPid),
  true = erlang:demonitor(CMon),
  true = erlang:demonitor(FromMon),
  handle_next_wait(State).

-spec handle_next_wait(State :: errm_driver_state()) -> errm_driver_state().
handle_next_wait(State=#errm_driver_state{ waiting=Q }) ->
  case queue:out(Q) of
    {empty, Q} ->
      State;
    {{value, #errm_driver_wait{ from=From, conn_type=ConnType, timeout=Timeout }}, Q2} ->
      {ok, State2} = handle_acquire(From, ConnType, Timeout, front, State#errm_driver_state{ waiting=Q2 }),
      State2
  end.

-spec handle_change_from(
  NFrom :: gen_server:from(),
  ConnMon :: errm_conn_mon(),
  State :: errm_driver_state()
) -> errm_driver_state().
handle_change_from(
  NFrom={NFromPid, _},
  #errm_conn_mon{
    connection=Conn,
    connection_monitor=ConnMonitorRef,
    from=_,
    from_monitor=FromMonitorRef,
    conn_type=ConnType
  },
  State=#errm_driver_state{ conn_monitors=C }
) ->
  true = erlang:demonitor(FromMonitorRef),
  NFromMonitorRef = erlang:monitor(process, NFromPid),
  true = ets:insert(C, #errm_conn_mon{
    id=Conn,
    type=connection,
    connection=Conn,
    connection_monitor=ConnMonitorRef,
    from=NFrom,
    from_monitor=NFromMonitorRef,
    conn_type=ConnType
  }),
  true = ets:insert(C, #errm_conn_mon{
    id=NFromPid,
    type=from,
    connection=Conn,
    connection_monitor=ConnMonitorRef,
    from=NFrom,
    from_monitor=NFromMonitorRef,
    conn_type=ConnType
  }),
  State.