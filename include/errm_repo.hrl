
-record(errm_repo_init, {
  repo_supervisor_intensity :: pos_integer(),
  repo_supervisor_period :: pos_integer(),
  driver_server_name :: atom(),
  driver_module :: atom(),
  driver_args :: [term()],
  driver_pool_min :: pos_integer() | undefined,
  driver_pool_max :: pos_integer() | undefined,
  driver_shutdown :: pos_integer() | undefined,
  replica_enabled :: boolean() | undefined,
  replica_args :: [term()] | undefined,
  replica_pool_min :: pos_integer() | undefined,
  replica_pool_max :: pos_integer() | undefined,
  migration_server_name :: atom(),
  migration_table :: string(),
  migration_schema :: string(),
  migration_path :: string(),
  migration_shutdown :: pos_integer() | undefined,
  seed_server_name :: atom(),
  seed_table :: string(),
  seed_schema :: string(),
  seed_path :: string(),
  seed_shutdown :: pos_integer() | undefined
}).

% errm_repo start calls errm_sup to create repo
% errm_sup creates child of errm_repo_sup to manage driver, migrate, seed servers + create shorthand table
% errm_repo hits shorthand table to grab name of driver, migrate, seed servers to make subsequent calls with errm_driver, errm_connection, errm_migrate, errm_seed
% errm_repo stop calls errm_sup to delete repo

-type errm_repo_init() :: #errm_repo_init{}.

-record(errm_repo_state, {
  name :: atom(),
  sup :: pid(),
  table :: ets:table()
}).

-record(errm_repo_server, {
  name :: atom(),
  driver :: atom(),
  migrations :: atom(),
  seeds :: atom()
}).

-type errm_repo_server() :: #errm_repo_server{}.

-define(ERRM_REPO_DEFAULT, errm_repo_default).
-define(ERRM_REPO_DEFAULT_DRIVER, errm_repo_default_driver).
-define(ERRM_REPO_DEFAULT_MIGRATE, errm_repo_default_migrate).
-define(ERRM_REPO_DEFAULT_SEED, errm_repo_default_seed).
-define(ERRM_REPO_DEFAULT_SHUTDOWN, 5000).
-define(ERRM_REPO_DEFAULT_POOL_MIN, 1).
-define(ERRM_REPO_DEFAULT_POOL_MAX, 1).
-define(ERRM_REPO_DEFAULT_DRIVER_ENABLED, false).
-define(ERRM_REPO_DEFAULT_REPLICA_ENABLED, false).

-define(ERRM_REPO_TABLE, errm_repo_table).