-module(errm_connection_tests).

-include_lib("eunit/include/eunit.hrl").
-include("errm_connection.hrl").
-include("errm_query.hrl").
-include("errm_error.hrl").

% start_link

% stop

% acquire

acquire_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun acquire_test_run/1
  }.

acquire_fail_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun acquire_fail_test_run/1
  }.

% release

release_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun release_test_run/1
  }.

release_fail_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun release_fail_test_run/1
  }.

% query

query_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun query_test_run/1
  }.

query_param_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun query_test_param_run/1
  }.

query_multi_param_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun query_test_multi_param_run/1
  }.

query_tx_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun query_test_tx_run/1
  }.

% start transaction

start_transaction_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun start_transaction_run/1
  }.

start_sub_transaction_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun start_sub_transaction_run/1
  }.

% query transaction

query_transaction_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun query_transaction_run/1
  }.

query_transaction_param_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun query_transaction_param_run/1
  }.

query_transaction_multi_param_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun query_transaction_multi_param_run/1
  }.

query_transaction_no_tx_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun query_transaction_no_tx_run/1
  }.

query_transaction_wrong_tx_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun query_transaction_wrong_tx_run/1
  }.

% commit transaction

commit_transaction_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun commit_transaction_run/1
  }.

commit_transaction_sub_tx_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun commit_transaction_sub_tx_run/1
  }.

commit_transaction_no_tx_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun commit_transaction_no_tx_run/1
  }.

commit_transaction_wrong_tx_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun commit_transaction_wrong_tx_run/1
  }.

% rollback transaction

rollback_transaction_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun rollback_transaction_run/1
  }.

rollback_transaction_sub_tx_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun rollback_transaction_sub_tx_run/1
  }.

rollback_transaction_no_tx_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun rollback_transaction_no_tx_run/1
  }.

rollback_transaction_wrong_tx_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun rollback_transaction_wrong_tx_run/1
  }.

% test functions

start() ->
  meck:new(errm_sqlite_driver, [passthrough]),
  {ok, Pid} = errm_connection:start_link(#errm_connection_init{ module=errm_sqlite_driver, args=[":memory:"] }),
  Pid.

stop(Pid) ->
  true = meck:validate(errm_sqlite_driver),
  meck:unload(errm_sqlite_driver),
  case is_process_alive(Pid) of
    true -> errm_connection:stop(Pid);
    false -> ok
  end.

acquire_test_run(Pid) ->
  ?_assertEqual(errm_connection:acquire(Pid), ok).

acquire_fail_test_run(Pid) ->
  Out = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef}, Out),
  {ok, TxRef} = Out,
  ?assertEqual(true, is_reference(TxRef)),
  ?assertEqual(true, is_process_alive(Pid)),
  ?assertMatch({error, #errm_error{ code=?ERRM_TRANSACTION_LEAK_ERROR }}, errm_connection:acquire(Pid)),
  ?_assertEqual(false, is_process_alive(Pid)).

release_test_run(Pid) ->
  ?_assertEqual(ok, errm_connection:release(Pid)).

release_fail_test_run(Pid) ->
  Out = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef}, Out),
  {ok, TxRef} = Out,
  ?assertEqual(true, is_reference(TxRef)),
  ?assertEqual(true, is_process_alive(Pid)),
  ?assertMatch({error, #errm_error{ code=?ERRM_TRANSACTION_LEAK_ERROR }}, errm_connection:release(Pid)),
  ?_assertEqual(false, is_process_alive(Pid)).

query_test_run(Pid) ->
  meck:expect(errm_sqlite_driver, query, fun(Driver, Query, Params, Timeout) ->
    ?assertEqual("select 1;", Query),
    ?assertEqual([], Params),
    meck:passthrough([Driver, Query, Params, Timeout])
  end),
  Out = errm_connection:query(Pid, "select 1;", #errm_query_options{ parameters=[] }),
  ?assertEqual(1, meck:num_calls(errm_sqlite_driver, query, '_')),
  ?_assertEqual({ok, [[1]]}, Out).

query_test_param_run(Pid) ->
  meck:expect(errm_sqlite_driver, query, fun(Driver, Query, Params, Timeout) ->
    ?assertEqual("select ?;", Query),
    ?assertEqual([1], Params),
    meck:passthrough([Driver, Query, Params, Timeout])
  end),
  Out = errm_connection:query(Pid, "select ?;", #errm_query_options{ parameters=[1] }),
  ?assertEqual(1, meck:num_calls(errm_sqlite_driver, query, '_')),
  ?_assertEqual({ok, [[1]]}, Out).

query_test_multi_param_run(Pid) ->
  meck:expect(errm_sqlite_driver, query, fun(Driver, Query, Params, Timeout) ->
    ?assertEqual("select ?, ?;", Query),
    ?assertEqual([1, "2"], Params),
    meck:passthrough([Driver, Query, Params, Timeout])
  end),
  Out = errm_connection:query(Pid, "select ?, ?;", #errm_query_options{ parameters=[1, "2"] }),
  ?assertEqual(1, meck:num_calls(errm_sqlite_driver, query, '_')),
  ?_assertEqual({ok, [[1, <<"2">>]]}, Out).

query_test_tx_run(Pid) ->
  StartOut = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef}, StartOut),
  Out = errm_connection:query(Pid, "select 1;", #errm_query_options{ parameters=[] }),
  ?assertEqual(0, meck:num_calls(errm_sqlite_driver, query, '_')),
  ?_assertMatch({error, #errm_error{}}, Out).

start_transaction_run(Pid) ->
  meck:expect(errm_sqlite_driver, start_transaction, fun (Driver, Level, Timeout) ->
    ?assertEqual("read committed", Level),
    meck:passthrough([Driver, Level, Timeout])
  end),
  Out = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef}, Out),
  {ok, TxRef} = Out,
  ?assertEqual(1, meck:num_calls(errm_sqlite_driver, start_transaction, '_')),
  ?_assertEqual(true, is_reference(TxRef)).

start_sub_transaction_run(Pid) ->
  StartOut = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef}, StartOut),
  {ok, TxRef} = StartOut,
  meck:expect(errm_sqlite_driver, savepoint, fun (Driver, Savepoint, Timeout) ->
    ?assertEqual([?ERRM_CONN_SAVEPOINT, "1"], Savepoint),
    meck:passthrough([Driver, Savepoint, Timeout])
  end),
  {ok, TxRef2} = errm_connection:start_transaction(Pid, "read committed"),
  ?assertEqual(1, meck:num_calls(errm_sqlite_driver, savepoint, '_')),
  ?_assert(TxRef =/= TxRef2).

query_transaction_run(Pid) ->
  StartOut = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef}, StartOut),
  {ok, TxRef} = StartOut,
  meck:expect(errm_sqlite_driver, query, fun (Driver, Query, Params, Timeout) ->
    ?assertEqual("select 1;", Query),
    ?assertEqual([], Params),
    meck:passthrough([Driver, Query, Params, Timeout])
  end),
  Out = errm_connection:query(Pid, "select 1;", #errm_query_options{ parameters=[], transaction=TxRef }),
  ?assertEqual(1, meck:num_calls(errm_sqlite_driver, query, '_')),
  ?_assertEqual({ok, [[1]]}, Out).

query_transaction_param_run(Pid) ->
  StartOut = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef}, StartOut),
  {ok, TxRef} = StartOut,
  meck:expect(errm_sqlite_driver, query, fun(Driver, Query, Params, Timeout) ->
    ?assertEqual("select ?;", Query),
    ?assertEqual([1], Params),
    meck:passthrough([Driver, Query, Params, Timeout])
  end),
  Out = errm_connection:query(Pid, "select ?;", #errm_query_options{ parameters=[1], transaction=TxRef }),
  ?assertEqual(1, meck:num_calls(errm_sqlite_driver, query, '_')),
  ?_assertEqual({ok, [[1]]}, Out).

query_transaction_multi_param_run(Pid) ->
  StartOut = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef}, StartOut),
  {ok, TxRef} = StartOut,
  meck:expect(errm_sqlite_driver, query, fun(Driver, Query, Params, Timeout) ->
    ?assertEqual("select ?, ?;", Query),
    ?assertEqual([1, "2"], Params),
    meck:passthrough([Driver, Query, Params, Timeout])
  end),
  Out = errm_connection:query(Pid, "select ?, ?;", #errm_query_options{ parameters=[1, "2"], transaction=TxRef }),
  ?assertEqual(1, meck:num_calls(errm_sqlite_driver, query, '_')),
  ?_assertEqual({ok, [[1, <<"2">>]]}, Out).

query_transaction_no_tx_run(Pid) ->
  Out = errm_connection:query(Pid, "select 1;", #errm_query_options{ parameters=[], transaction=make_ref() }),
  ?assertEqual(0, meck:num_calls(errm_sqlite_driver, query, '_')),
  ?_assertMatch({error, #errm_error{}}, Out).

query_transaction_wrong_tx_run(Pid) ->
  StartOut = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef}, StartOut),
  Out = errm_connection:query(Pid, "select 1;", #errm_query_options{ parameters=[], transaction=make_ref() }),
  ?assertEqual(0, meck:num_calls(errm_sqlite_driver, query, '_')),
  ?_assertMatch({error, #errm_error{}}, Out).

commit_transaction_run(Pid) ->
  StartOut = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef}, StartOut),
  {ok, TxRef} = StartOut,
  ?assertEqual(ok, errm_connection:commit_transaction(Pid, TxRef)),
  ?_assertEqual(1, meck:num_calls(errm_sqlite_driver, commit_transaction, '_')).

commit_transaction_sub_tx_run(Pid) ->
  ?assertMatch({ok, _TxRef}, errm_connection:start_transaction(Pid, "read committed")),
  StartOut = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef2}, StartOut),
  {ok, TxRef2} = StartOut,
  meck:expect(errm_sqlite_driver, release_savepoint, fun (Driver, Savepoint, Timeout) ->
    ?assertEqual([?ERRM_CONN_SAVEPOINT, "1"], Savepoint),
    meck:passthrough([Driver, Savepoint, Timeout])
  end),
  Out = errm_connection:commit_transaction(Pid, TxRef2),
  ?assertEqual(1, meck:num_calls(errm_sqlite_driver, release_savepoint, '_')),
  ?_assertEqual(ok, Out).

commit_transaction_no_tx_run(Pid) ->
  Out = errm_connection:commit_transaction(Pid, erlang:make_ref()),
  ?assertEqual(0, meck:num_calls(errm_sqlite_driver, commit_transaction, '_')),
  ?_assertMatch({error, #errm_error{}}, Out).

commit_transaction_wrong_tx_run(Pid) ->
  ?assertMatch({ok, _TxRef}, errm_connection:start_transaction(Pid, "read committed")),
  Out = errm_connection:commit_transaction(Pid, erlang:make_ref()),
  ?assertEqual(0, meck:num_calls(errm_sqlite_driver, commit_transaction, '_')),
  ?_assertMatch({error, #errm_error{}}, Out).

rollback_transaction_run(Pid) ->
  StartOut = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef}, StartOut),
  {ok, TxRef} = StartOut,
  ?assertEqual(ok, errm_connection:rollback_transaction(Pid, TxRef)),
  ?_assertEqual(1, meck:num_calls(errm_sqlite_driver, rollback_transaction, '_')).

rollback_transaction_sub_tx_run(Pid) ->
  ?assertMatch({ok, _TxRef}, errm_connection:start_transaction(Pid, "read committed")),
  StartOut = errm_connection:start_transaction(Pid, "read committed"),
  ?assertMatch({ok, _TxRef2}, StartOut),
  {ok, TxRef2} = StartOut,
  meck:expect(errm_sqlite_driver, revert_savepoint, fun (Driver, Savepoint, Timeout) ->
    ?assertEqual([?ERRM_CONN_SAVEPOINT, "1"], Savepoint),
    meck:passthrough([Driver, Savepoint, Timeout])
  end),
  Out = errm_connection:rollback_transaction(Pid, TxRef2),
  ?assertEqual(1, meck:num_calls(errm_sqlite_driver, revert_savepoint, '_')),
  ?_assertEqual(ok, Out).

rollback_transaction_no_tx_run(Pid) ->
  Out = errm_connection:rollback_transaction(Pid, erlang:make_ref()),
  ?assertEqual(0, meck:num_calls(errm_sqlite_driver, rollback, '_')),
  ?_assertMatch({error, #errm_error{}}, Out).

rollback_transaction_wrong_tx_run(Pid) ->
  ?assertMatch({ok, _TxRef}, errm_connection:start_transaction(Pid, "read committed")),
  Out = errm_connection:rollback_transaction(Pid, erlang:make_ref()),
  ?assertEqual(0, meck:num_calls(errm_sqlite_driver, rollback, '_')),
  ?_assertMatch({error, #errm_error{}}, Out).