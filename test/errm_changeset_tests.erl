-module(errm_changeset_tests).

-include_lib("eunit/include/eunit.hrl").
-include("errm_changeset.hrl").
-include("errm_query.hrl").
-include("errm_driver.hrl").

-define(ERRM_DRIVER, errm_driver).

% start

start_link_create_changeset_table_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun start_link_create_changeset_table_test_run/1
  }.

start_link_load_changes_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun start_link_load_changes_test_run/1
  }.

% up all

up_all_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun up_all_test_run/1
  }.

% up id

up_first_change_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun up_first_change_test_run/1
  }.

up_changes_one_by_one_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun up_changes_one_by_one_test_run/1
  }.

% up repeat

% down all

down_all_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun down_all_test_run/1
  }.

% down id

down_last_change_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun down_last_change_test_run/1
  }.

down_changes_one_by_one_test_() ->
  {
    setup,
    fun start/0,
    fun stop/1,
    fun down_changes_one_by_one_test_run/1
  }.

% test functions

start() ->
  application:ensure_all_started(yamerl),
  meck:new(errm_driver, [passthrough]),
  meck:new(errm_connection, [passthrough]),
  meck:new(errm_changeset, [passthrough]),

  Mod = errm_sqlite_driver,
  Args = ["file:memdb?mode=memory&cache=shared"],
  PoolOpts = #errm_pool_opts{ min=1, max=1 },
  {ok, _DriverPid} = errm_driver:start_link(
    ?ERRM_DRIVER,
    #errm_driver_init{
      module=Mod,
      args=Args,
      pool_opts=PoolOpts,
      replica_opts=undefined
    }
  ),

  {ok, CWD} = file:get_cwd(),
  Path = filename:join([CWD, "test/migrations"]),
  Schema = "main",
  TableName = "errm_migrations",
  {ok, Pid} = errm_changeset:start_link(#errm_changeset_init{
    path=Path,
    schema=Schema,
    table_name=TableName,
    driver=?ERRM_DRIVER
  }),
  Pid.

stop(Pid) ->
  true = meck:validate(errm_driver),
  true = meck:validate(errm_connection),
  true = meck:validate(errm_changeset),

  ok = meck:unload(errm_driver),
  ok = meck:unload(errm_connection),
  ok = meck:unload(errm_changeset),

  errm_changeset:stop(Pid),
  errm_driver:stop(?ERRM_DRIVER).

start_link_create_changeset_table_test_run(_Pid) ->
  {ok, Conn} = errm_driver:get_connection(?ERRM_DRIVER),
  Query = ?STMT_F([
    ?SELECT_FROM(["*"], "sqlite_master"),
    ?WHERE(?EQ("name", "'errm_migrations'"))
  ]),
  QueryRes = errm_connection:query(Conn, Query),
  ?assertMatch({ok, [_]}, QueryRes),
  {ok, [TableDef]} = QueryRes,

  ?_assertMatch(
    [
      <<"table">>,
      <<"errm_migrations">>,
      <<"errm_migrations">>,
      _,
      <<"CREATE TABLE errm_migrations (name TEXT PRIMARY KEY)">>
    ],
    TableDef
  ).

start_link_load_changes_test_run(Pid) ->
  {ok, Changes} = errm_changeset:status(Pid),
  ?assertEqual(2, length(Changes)),
  ?assertMatch([#errm_changeset{}, #errm_changeset{}], Changes),
  [Change1, Change2] = Changes,

  ?assertEqual(
    #errm_changeset{
      name="0001_create_table_test1",
      up_sql="CREATE TABLE IF NOT EXISTS test1 ( name TEXT PRIMARY KEY, attribute_one INTEGER, attribute_two TEXT );",
      up_options=#errm_changeset_options{ transaction=true, timeout=5000 },
      down_sql="DROP TABLE IF EXISTS test1;",
      down_options=#errm_changeset_options{ transaction=true, timeout=5000 },
      status=unknown
    },
    Change1
  ),
  ?_assertEqual(
    #errm_changeset{
      name="0002_alter_table_test1",
      up_sql="ALTER TABLE test1 ADD COLUMN attribute_three TEXT; CREATE UNIQUE INDEX IF NOT EXISTS UI_0002 ON test1(attribute_three);",
      up_options=#errm_changeset_options{ transaction=true, timeout=6000 },
      down_sql="DROP INDEX IF EXISTS UI_0002; ALTER TABLE test1 DROP COLUMN attribute_three;",
      down_options=#errm_changeset_options{ transaction=true, timeout=7000 },
      status=unknown
    },
    Change2
  ).

up_all_test_run(Pid) ->
  Out = errm_changeset:up(Pid, all),
  ?assertEqual(ok, Out),

  {ok, Conn} = errm_driver:get_connection(?ERRM_DRIVER),

  Query = ?STMT_F([
    ?SELECT_FROM(["*"], "sqlite_master"),
    ?WHERE(?STMT([
      ?EQ("name", "'test1'"),
      ?OR(?EQ("name", "'UI_0002'"))
    ]))
  ]),
  QueryRes = errm_connection:query(Conn, Query),
  ?assertMatch({ok, [_, _]}, QueryRes),
  {ok, [Tab1Def, Ind1Def]} = QueryRes,

  ?assertMatch(
    [
      <<"table">>,
      <<"test1">>,
      <<"test1">>,
      _,
      <<"CREATE TABLE test1 ( name TEXT PRIMARY KEY, attribute_one INTEGER, attribute_two TEXT , attribute_three TEXT)">>
    ],
    Tab1Def
  ),
  ?assertMatch(
    [
      <<"index">>,
      <<"UI_0002">>,
      <<"test1">>,
      _,
      <<"CREATE UNIQUE INDEX UI_0002 ON test1(attribute_three)">>
    ],
    Ind1Def
  ),

  ?_assertEqual(
    {ok, [[<<"0001_create_table_test1">>], [<<"0002_alter_table_test1">>]]},
    errm_connection:query(Conn, ?STMT_F([?SELECT_FROM(["*"], "errm_migrations")]))
  ).

up_first_change_test_run(Pid) ->
  Out = errm_changeset:up(Pid, "0001_create_table_test1"),
  ?assertEqual(ok, Out),

  {ok, Conn} = errm_driver:get_connection(?ERRM_DRIVER),

  Query = ?STMT_F([
    ?SELECT_FROM(["*"], "sqlite_master"),
    ?WHERE(?STMT([
      ?EQ("name", "'test1'"),
      ?OR(?EQ("name", "'UI_0002'"))
    ]))
  ]),
  QueryRes = errm_connection:query(Conn, Query),
  ?assertMatch({ok, [_]}, QueryRes),
  {ok, [Tab1Def]} = QueryRes,

  ?assertMatch(
    [
      <<"table">>,
      <<"test1">>,
      <<"test1">>,
      _,
      <<"CREATE TABLE test1 ( name TEXT PRIMARY KEY, attribute_one INTEGER, attribute_two TEXT )">>
    ],
    Tab1Def
  ),

  ?_assertEqual(
    {ok, [[<<"0001_create_table_test1">>]]},
    errm_connection:query(Conn, ?STMT_F([?SELECT_FROM(["*"], "errm_migrations")]))
  ).

up_changes_one_by_one_test_run(Pid) ->
  Out = errm_changeset:up(Pid, "0001_create_table_test1"),
  ?assertEqual(ok, Out),

  {ok, Conn} = errm_driver:get_connection(?ERRM_DRIVER),

  Query = ?STMT_F([
    ?SELECT_FROM(["*"], "sqlite_master"),
    ?WHERE(?STMT([
      ?EQ("name", "'test1'"),
      ?OR(?EQ("name", "'UI_0002'"))
    ]))
  ]),
  QueryRes = errm_connection:query(Conn, Query),
  ?assertMatch({ok, [_]}, QueryRes),
  {ok, [Tab1Def]} = QueryRes,

  ?assertMatch(
    [
      <<"table">>,
      <<"test1">>,
      <<"test1">>,
      _,
      <<"CREATE TABLE test1 ( name TEXT PRIMARY KEY, attribute_one INTEGER, attribute_two TEXT )">>
    ],
    Tab1Def
  ),

  ?assertEqual(
    {ok, [[<<"0001_create_table_test1">>]]},
    errm_connection:query(Conn, ?STMT_F([?SELECT_FROM(["*"], "errm_migrations")]))
  ),

  ok = errm_driver:release_connection(?ERRM_DRIVER, Conn),

  Out2 = errm_changeset:up(Pid, "0002_alter_table_test1"),
  ?assertEqual(ok, Out2),

  {ok, Conn2} = errm_driver:get_connection(?ERRM_DRIVER),

  QueryRes2 = errm_connection:query(Conn2, Query),
  ?assertMatch({ok, [_, _]}, QueryRes2),
  {ok, [Tab1Def2, Ind1Def]} = QueryRes2,

  ?assertMatch(
    [
      <<"table">>,
      <<"test1">>,
      <<"test1">>,
      _,
      <<"CREATE TABLE test1 ( name TEXT PRIMARY KEY, attribute_one INTEGER, attribute_two TEXT , attribute_three TEXT)">>
    ],
    Tab1Def2
  ),
  ?assertMatch(
    [
      <<"index">>,
      <<"UI_0002">>,
      <<"test1">>,
      _,
      <<"CREATE UNIQUE INDEX UI_0002 ON test1(attribute_three)">>
    ],
    Ind1Def
  ),

  ?_assertEqual(
    {ok, [[<<"0001_create_table_test1">>], [<<"0002_alter_table_test1">>]]},
    errm_connection:query(Conn2, ?STMT_F([?SELECT_FROM(["*"], "errm_migrations")]))
  ).

down_all_test_run(Pid) ->
  Up = errm_changeset:up(Pid, all),
  ?assertEqual(ok, Up),

  Down = errm_changeset:down(Pid, all),
  ?assertEqual(ok, Down),

  {ok, Conn} = errm_driver:get_connection(?ERRM_DRIVER),

  QueryRes = errm_connection:query(Conn, ?STMT_F([?SELECT_FROM(["*"], "sqlite_master")])),
  ?assertMatch({ok, [_, _]}, QueryRes),
  {ok, [ChangeTDef, _]} = QueryRes,

  ?assertMatch(
    [
      <<"table">>,
      <<"errm_migrations">>,
      <<"errm_migrations">>,
      _,
      <<"CREATE TABLE errm_migrations (name TEXT PRIMARY KEY)">>
    ],
    ChangeTDef
  ),

  ?_assertEqual(
    {ok, []},
    errm_connection:query(Conn, ?STMT_F([?SELECT_FROM(["*"], "errm_migrations")]))
  ).

down_last_change_test_run(Pid) ->
  Up = errm_changeset:up(Pid, all),
  ?assertEqual(ok, Up),

  Down = errm_changeset:down(Pid, "0002_alter_table_test1"),
  ?assertEqual(ok, Down),

  {ok, Conn} = errm_driver:get_connection(?ERRM_DRIVER),

  Query = ?STMT_F([
    ?SELECT_FROM(["*"], "sqlite_master"),
    ?WHERE(?STMT([
      ?EQ("name", "'test1'"),
      ?OR(?EQ("name", "'UI_0002'"))
    ]))
  ]),
  QueryRes = errm_connection:query(Conn, Query),
  ?assertMatch({ok, [_]}, QueryRes),
  {ok, [Tab1Def]} = QueryRes,

  ?assertMatch(
    [
      <<"table">>,
      <<"test1">>,
      <<"test1">>,
      _,
      <<"CREATE TABLE test1 ( name TEXT PRIMARY KEY, attribute_one INTEGER, attribute_two TEXT )">>
    ],
    Tab1Def
  ),

  ?_assertEqual(
    {ok, [[<<"0001_create_table_test1">>]]},
    errm_connection:query(Conn, ?STMT_F([?SELECT_FROM(["*"], "errm_migrations")]))
  ).

down_changes_one_by_one_test_run(Pid) ->
  Up = errm_changeset:up(Pid, all),
  ?assertEqual(ok, Up),

  Down = errm_changeset:down(Pid, "0002_alter_table_test1"),
  ?assertEqual(ok, Down),

  {ok, Conn} = errm_driver:get_connection(?ERRM_DRIVER),

  Query = ?STMT_F([
    ?SELECT_FROM(["*"], "sqlite_master"),
    ?WHERE(?STMT([
      ?EQ("name", "'test1'"),
      ?OR(?EQ("name", "'UI_0002'"))
    ]))
  ]),
  QueryRes = errm_connection:query(Conn, Query),
  ?assertMatch({ok, [_]}, QueryRes),
  {ok, [Tab1Def]} = QueryRes,

  ?assertMatch(
    [
      <<"table">>,
      <<"test1">>,
      <<"test1">>,
      _,
      <<"CREATE TABLE test1 ( name TEXT PRIMARY KEY, attribute_one INTEGER, attribute_two TEXT )">>
    ],
    Tab1Def
  ),

  ?assertEqual(
    {ok, [[<<"0001_create_table_test1">>]]},
    errm_connection:query(Conn, ?STMT_F([?SELECT_FROM(["*"], "errm_migrations")]))
  ),

  ok = errm_driver:release_connection(?ERRM_DRIVER, Conn),

  Down2 = errm_changeset:down(Pid, "0001_create_table_test1"),
  ?assertEqual(ok, Down2),

  {ok, Conn2} = errm_driver:get_connection(?ERRM_DRIVER),

  QueryRes2 = errm_connection:query(Conn2, Query),
  ?assertMatch({ok, []}, QueryRes2),

  ?_assertEqual(
    {ok, []},
    errm_connection:query(Conn, ?STMT_F([?SELECT_FROM(["*"], "errm_migrations")]))
  ).