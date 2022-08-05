
-record(errm_pool_opts, { min :: non_neg_integer(), max :: non_neg_integer() }).

-type errm_pool_opts() :: #errm_pool_opts{}.

-record(errm_replica_init, {
  module :: module(),
  args :: [term()],
  pool_opts :: errm_pool_opts()
}).

-type errm_replica_init() :: #errm_replica_init{}.

-record(errm_driver_init, {
  module :: module(),
  args :: [term()],
  pool_opts :: errm_pool_opts(),
  replica_opts :: errm_replica_init() | undefined
}).

-type errm_driver_init() :: #errm_driver_init{}.

-record(errm_driver_state, {
  pool :: pid(),
  replica_pool :: pid() | undefined,
  waiting :: queue:queue(),
  conn_monitors :: ets:table()
}).

-type errm_driver_state() :: #errm_driver_state{}.

-type errm_connection_monitor_type() :: from | connection.

-type errm_connection_type() :: main | replica.

-record(errm_conn_mon, {
  id :: pid(),
  type :: errm_connection_monitor_type(),
  connection :: pid(),
  connection_monitor :: reference(),
  from :: gen_server:from(),
  from_monitor :: reference(),
  conn_type :: errm_connection_type()
}).

-type errm_conn_mon() :: #errm_conn_mon{}.

-record(errm_driver_wait, {
  from :: gen_server:from(),
  conn_type :: errm_connection_type(),
  timeout :: pos_integer()
}).

-type errm_driver_wait() :: #errm_driver_wait{}.

-define(ERRM_DRIVER_CONNECTION_MONITORS, errm_driver_connection_monitors).
-define(ERRM_DRIVER_DEFAULT_TIMEOUT, 5000).