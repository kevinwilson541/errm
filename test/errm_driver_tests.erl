-module(errm_driver_tests).

-include_lib("eunit/include/eunit.hrl").
-include("errm_driver.hrl").
-include("errm_error.hrl").
-include("errm_connection.hrl").

% get_connection

get_connection_main_test_() ->
  {
    setup,
    fun start_wo_replica/0,
    fun stop/1,
    fun get_connection_main_test_run/1
  }.

get_connection_replica_test_() ->
  {
    setup,
    fun start_with_replica/0,
    fun stop/1,
    fun get_connection_replica_test_run/1
  }.

get_connection_main_multiple_test_() ->
  {
    setup,
    fun start_wo_replica/0,
    fun stop/1,
    fun get_connection_main_multi_test_run/1
  }.

get_connection_replica_multiple_test_() ->
  {
    setup,
    fun start_with_replica/0,
    fun stop/1,
    fun get_connection_replica_multi_test_run/1
  }.

get_connection_down_release_test_() ->
  {
    setup,
    fun start_wo_replica/0,
    fun stop/1,
    fun get_connection_down_release_test_run/1
  }.

get_connection_from_down_release_test_() ->
  {
    setup,
    fun start_wo_replica/0,
    fun stop/1,
    fun get_connection_from_down_release_test_run/1
  }.

get_connection_on_down_release_test_() ->
  {
    setup,
    fun start_wo_replica/0,
    fun stop/1,
    fun get_connection_on_down_release_test_run/1
  }.

% release_connection

release_connection_main_test_() ->
  {
    setup,
    fun start_wo_replica/0,
    fun stop/1,
    fun release_connection_main_test_run/1
  }.

release_connection_replica_test_() ->
  {
    setup,
    fun start_with_replica/0,
    fun stop/1,
    fun release_connection_replica_test_run/1
  }.

% test functions

start_with_replica() ->
  meck:new(poolboy, [passthrough]),
  meck:new(errm_connection, [passthrough]),
  meck:new(errm_driver, [passthrough]),
  Mod = errm_sqlite_driver,
  Args = [":memory:"],
  PoolOpts = #errm_pool_opts{ min=1, max=1 },
  ReplicaOpts = #errm_replica_init{ module=Mod, args=Args, pool_opts=PoolOpts },
  {ok, Pid} = errm_driver:start_link(#errm_driver_init{
    module=Mod,
    args=Args,
    pool_opts=PoolOpts,
    replica_opts=ReplicaOpts
  }),
  Pid.

start_wo_replica() ->
  meck:new(poolboy, [passthrough]),
  meck:new(errm_connection, [passthrough]),
  meck:new(errm_driver, [passthrough]),
  Mod = errm_sqlite_driver,
  Args = [":memory:"],
  PoolOpts = #errm_pool_opts{ min=1, max=1 },
  {ok, Pid} = errm_driver:start_link(#errm_driver_init{
    module=Mod,
    args=Args,
    pool_opts=PoolOpts,
    replica_opts=undefined
  }),
  Pid.

stop(Pid) ->
  true = meck:validate(poolboy),
  true = meck:validate(errm_connection),
  true = meck:validate(errm_driver),
  ok = meck:unload(poolboy),
  ok = meck:unload(errm_connection),
  ok = meck:unload(errm_driver),
  errm_driver:stop(Pid).

get_connection_main_test_run(Pid) ->
  Out = errm_driver:get_connection(Pid),
  ?assertEqual(1, meck:num_calls(poolboy, checkout, '_')),
  ?assertMatch({ok, _Conn}, Out),
  {ok, Conn} = Out,

  ?assertEqual(2, length(ets:tab2list(?ERRM_DRIVER_CONNECTION_MONITORS))),
  ConnMon = ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, Conn),
  ?assertMatch([#errm_conn_mon{ id=Conn, type=connection, connection=Conn, from={_, _}, conn_type=main }], ConnMon),

  [#errm_conn_mon{ id=Conn, from={FromPid, FromRef}, connection_monitor=CRef }] = ConnMon,

  FromMon = ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, FromPid),
  ?_assertMatch([#errm_conn_mon{
    id=FromPid,
    type=from,
    connection=Conn,
    connection_monitor=CRef,
    from={FromPid, FromRef},
    conn_type=main
  }], FromMon).

get_connection_replica_test_run(Pid) ->
  Out = errm_driver:get_connection(Pid, replica),
  ?assertEqual(1, meck:num_calls(poolboy, checkout, '_')),
  ?assertMatch({ok, _Conn}, Out),
  {ok, Conn} = Out,

  ?assertEqual(2, length(ets:tab2list(?ERRM_DRIVER_CONNECTION_MONITORS))),
  ConnMon = ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, Conn),
  ?assertMatch([#errm_conn_mon{ id=Conn, type=connection, connection=Conn, from={_, _}, conn_type=replica }], ConnMon),

  [#errm_conn_mon{ id=Conn, from={FromPid, FromRef}, connection_monitor=CRef }] = ConnMon,

  FromMon = ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, FromPid),
  ?_assertMatch([#errm_conn_mon{
    id=FromPid,
    type=from,
    connection=Conn,
    connection_monitor=CRef,
    from={FromPid, FromRef},
    conn_type=replica
  }], FromMon).

get_connection_main_multi_test_run(Pid) ->
  Out = errm_driver:get_connection(Pid),
  ?assertEqual(1, meck:num_calls(poolboy, checkout, '_')),
  ?assertMatch({ok, _Conn}, Out),
  {ok, Conn} = Out,

  Self = erlang:self(),

  RPid = erlang:spawn(fun () ->
    % wait until next checkout call happens, then release current context
    meck:wait(poolboy, checkout, '_', 5000),
    errm_driver:release_connection(Pid, Conn),
    % send message to test pid that release has happened
    Self ! {done, erlang:self()}
  end),

  Out2 = errm_driver:get_connection(Pid),

  receive
    {done, RPid} -> ok
  end,

  % once for first get_connection, once for second get_connection that makes second caller wait, once for notification
  % of release and subsequent call to get connection on waiting context
  ?assertEqual(3, meck:num_calls(poolboy, checkout, '_')),
  ?assertEqual(1, meck:num_calls(errm_connection, release, [Conn])),
  ?_assertMatch({ok, Conn}, Out2).

get_connection_replica_multi_test_run(Pid) ->
  Out = errm_driver:get_connection(Pid, replica),
  ?assertEqual(1, meck:num_calls(poolboy, checkout, '_')),
  ?assertMatch({ok, _Conn}, Out),
  {ok, Conn} = Out,

  Self = erlang:self(),

  RPid = erlang:spawn(fun () ->
    % wait until next checkout call happens, then release current context
    meck:wait(poolboy, checkout, '_', 5000),
    errm_driver:release_connection(Pid, Conn),
    % send message to test pid that release has happened
    Self ! {done, erlang:self()}
  end),

  Out2 = errm_driver:get_connection(Pid, replica),

  receive
    {done, RPid} -> ok
  end,

  % once for first get_connection, once for second get_connection that makes second caller wait, once for notification
  % of release and subsequent call to get connection on waiting context
  ?assertEqual(3, meck:num_calls(poolboy, checkout, '_')),
  ?assertEqual(1, meck:num_calls(errm_connection, release, [Conn])),
  ?_assertMatch({ok, Conn}, Out2).

get_connection_down_release_test_run(Pid) ->
  Out = errm_driver:get_connection(Pid, main),
  ?assertEqual(1, meck:num_calls(poolboy, checkout, '_')),
  ?assertMatch({ok, _Conn}, Out),
  {ok, Conn} = Out,

  [#errm_conn_mon{ id=Conn, connection_monitor=CRef }] = ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, Conn),

  % spy on call to down signal, handle it as module normally does, assert message matches stored monitors in ets table
  meck:expect(errm_driver, handle_info, fun (Info, State) ->
    ?assertEqual({'DOWN', CRef, process, Conn, normal}, Info),
    HandleInfoRet = meck:passthrough([Info, State]),
    ?assertEqual(State#errm_driver_state.waiting, queue:new()),
    HandleInfoRet
  end),

  gen_server:stop(Conn, normal, 5000),

  meck:wait(errm_driver, handle_info, '_', 5000),

  ?assertEqual(0, length(ets:tab2list(?ERRM_DRIVER_CONNECTION_MONITORS))),

  ?assertEqual(1, meck:num_calls(poolboy, checkout, '_')),
  ?assertEqual(0, meck:num_calls(poolboy, checkin, '_')),
  ?_assertEqual(1, meck:num_calls(errm_driver, handle_info, '_')).

get_connection_from_down_release_test_run(Pid) ->
  % setup spy on call to down signal, handle it as module normally does, assert message matches stored monitors in ets table
  meck:expect(errm_driver, handle_info, fun (Info, State) ->
    ?assertMatch({'DOWN', _, process, _, normal}, Info),
    {'DOWN', FRef, process, FPid, normal} = Info,

    ?assertMatch(
      [#errm_conn_mon{ id=FPid, type=from, from_monitor=FRef }],
      ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, FPid)
    ),

    {noreply, NState} = meck:passthrough([Info, State]),

    ?assertEqual(queue:new(), NState#errm_driver_state.waiting),

    {noreply, NState}
  end),

  % spawn process to manage call to grab connection, spawned process will exit after acquiring connection
  RPid = erlang:spawn(fun () ->
    Out = errm_driver:get_connection(Pid, main),
    ?assertEqual(1, meck:num_calls(poolboy, checkout, '_')),
    ?assertMatch({ok, _Conn}, Out),
    {ok, Conn} = Out,

    [#errm_conn_mon{ id=Conn, type=connection, from={FromPid, _} }] = ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, Conn),
    ?assertEqual(FromPid, erlang:self()),
    ?assertMatch([#errm_conn_mon{ id=FromPid, type=from, connection=Conn }], ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, FromPid)),

    erlang:exit(normal)
  end),

  % wait for call to handle down signal from spawned connection exit
  meck:wait(errm_driver, handle_info, [{'DOWN', '_', process, RPid, normal}, '_'], 5000),

  ?assertEqual(0, length(ets:tab2list(?ERRM_DRIVER_CONNECTION_MONITORS))),

  ?assertEqual(1, meck:num_calls(poolboy, checkout, '_')),
  ?assertEqual(1, meck:num_calls(poolboy, checkin, '_')),
  ?_assertEqual(1, meck:num_calls(errm_driver, handle_info, '_')).

get_connection_on_down_release_test_run(Pid) ->
  Self = erlang:self(),

  % setup spy on call to down signal, handle it as module normally does, assert message matches stored monitors in ets table
  meck:expect(errm_driver, handle_info, fun (Info, State) ->
    ?assertMatch({'DOWN', _, process, _, normal}, Info),
    {'DOWN', FRef, process, FPid, normal} = Info,

    ?assertMatch(
      [#errm_conn_mon{ id=FPid, type=from, from_monitor=FRef }],
      ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, FPid)
    ),

    ?assertEqual(1, queue:len(State#errm_driver_state.waiting)),

    {noreply, NState} = meck:passthrough([Info, State]),

    ?assertEqual(queue:new(), NState#errm_driver_state.waiting),

    {noreply, NState}
  end),

  % spawn process to manage call to grab connection, spawned process will exit after acquiring connection
  RPid = erlang:spawn(fun () ->
    Out = errm_driver:get_connection(Pid, main, 4000),
    ?assertMatch({ok, _Conn}, Out),
    {ok, Conn} = Out,

    [#errm_conn_mon{ id=Conn, type=connection, from={FromPid, _} }] = ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, Conn),
    ?assertEqual(FromPid, erlang:self()),
    ?assertMatch([#errm_conn_mon{ id=FromPid, type=from, connection=Conn }], ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, FromPid)),

    % wait for other attempt to get connection before exiting
    meck:wait(errm_driver, handle_call, [{get_connection, main, 5000}, '_', '_'], 5000),

    erlang:exit(normal)
  end),

  % wait for spawned process to grab connection
  meck:wait(errm_driver, get_connection, [Pid, main, 4000], 5000),
  % attempt to grab connection
  {ok, Conn} = errm_driver:get_connection(Pid, main, 5000),

  % wait for call to handle down signal from spawned connection exit for clean state
  meck:wait(errm_driver, handle_info, [{'DOWN', '_', process, RPid, normal}, '_'], 5000),

  ?assertEqual(2, length(ets:tab2list(?ERRM_DRIVER_CONNECTION_MONITORS))),

  ?assertMatch(
    [#errm_conn_mon{ id=Conn, type=connection, from={Self, _} }],
    ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, Conn)
  ),
  ?assertMatch(
    [#errm_conn_mon{ id=Self, type=from, connection=Conn }],
    ets:lookup(?ERRM_DRIVER_CONNECTION_MONITORS, Self)
  ),

  ?assertEqual(3, meck:num_calls(poolboy, checkout, '_')),
  ?assertEqual(1, meck:num_calls(poolboy, checkin, '_')),
  ?_assertEqual(1, meck:num_calls(errm_driver, handle_info, '_')).

release_connection_main_test_run(Pid) ->
  Out = errm_driver:get_connection(Pid, main),
  ?assertEqual(1, meck:num_calls(poolboy, checkout, '_')),
  ?assertMatch({ok, _Conn}, Out),
  {ok, Conn} = Out,

  ROut = errm_driver:release_connection(Pid, Conn),
  ?assertEqual(1, meck:num_calls(poolboy, checkin, '_')),
  ?assertMatch(ok, ROut),

  ?_assertEqual(0, length(ets:tab2list(?ERRM_DRIVER_CONNECTION_MONITORS))).

release_connection_replica_test_run(Pid) ->
  Out = errm_driver:get_connection(Pid, replica),
  ?assertEqual(1, meck:num_calls(poolboy, checkout, '_')),
  ?assertMatch({ok, _Conn}, Out),
  {ok, Conn} = Out,

  ROut = errm_driver:release_connection(Pid, Conn),
  ?assertEqual(1, meck:num_calls(poolboy, checkin, '_')),
  ?assertMatch(ok, ROut),

  ?_assertEqual(0, length(ets:tab2list(?ERRM_DRIVER_CONNECTION_MONITORS))).