-module(errm_repo_sup).

-include("errm_repo.hrl").
-include("errm_driver.hrl").
-include("errm_migrate.hrl").
-include("errm_seed.hrl").

-behavior(supervisor).

%% API
-export([
  start_link/1
]).
-export([
  init/1
]).

% @doc Start and link repository supervisor for driver, migration, and seed child processes.
% @param RepoInit   Initialization parameters of repo initialization for driver, migration, and seed child processes.
% @returns `{ok, pid()} | {error, term()}'
-spec start_link(RepoInit :: errm_repo_init()) -> {ok, pid()} | {error, term()}.
start_link(RepoInit) ->
  supervisor:start_link(?MODULE, RepoInit).

% supervisor callbacks

init(#errm_repo_init{
  repo_supervisor_intensity=SupIntensity,
  repo_supervisor_period=SupPeriod,
  driver_server_name=DriverName,
  driver_module=DriverMod,
  driver_args=DriverArgs,
  driver_pool_min=DriverPoolMin,
  driver_pool_max=DriverPoolMax,
  driver_shutdown=DriverShutdown,
  replica_enabled=ReplicaEnabled,
  replica_args=ReplicaArgs,
  replica_pool_min=ReplicaPoolMin,
  replica_pool_max=ReplicaPoolMax,
  migration_server_name=MigrationName,
  migration_table=MigrationTable,
  migration_schema=MigrationSchema,
  migration_path=MigrationPath,
  migration_shutdown=MigrationShutdown,
  seed_server_name=SeedName,
  seed_table=SeedTable,
  seed_schema=SeedSchema,
  seed_path=SeedPath,
  seed_shutdown=SeedShutdown
}) ->
  SupFlags = #{
    strategy => one_for_one,
    intensity => SupIntensity,
    period => SupPeriod
  },

  ReplicaOpts = case default_replica_enabled(ReplicaEnabled) of
                  false -> undefined;
                  true ->
                    #errm_replica_init{
                      module=DriverMod,
                      args=ReplicaArgs,
                      pool_opts=#errm_pool_opts{
                        min=default_pool_min(ReplicaPoolMin),
                        max=default_pool_max(ReplicaPoolMax)
                      }
                    }
                end,
  DriverInit = #errm_driver_init{
    module=DriverMod,
    args=DriverArgs,
    pool_opts=#errm_pool_opts{
      min=default_pool_min(DriverPoolMin),
      max=default_pool_max(DriverPoolMax)
    },
    replica_opts=ReplicaOpts
  },
  DriverSpec = #{
    id => errm_driver,
    start => {
      errm_driver,
      start_link,
      [DriverName, DriverInit]
    },
    restart => permanent,
    shutdown => default_shutdown(DriverShutdown),
    type => worker,
    modules => [errm_driver, errm_connection]
  },

  MigrateInit = #errm_migrate_init{
    path=MigrationPath,
    table_name=MigrationTable,
    schema=MigrationSchema,
    driver=DriverName
  },
  MigrateSpec = #{
    id => errm_migrate,
    start => {
      errm_migrate,
      start_link,
      [MigrationName, MigrateInit]
    },
    restart => permanent,
    shutdown => default_shutdown(MigrationShutdown),
    type => worker,
    modules => [errm_migrate, errm_changeset]
  },

  SeedInit = #errm_seed_init{
    path=SeedPath,
    table_name=SeedTable,
    schema=SeedSchema,
    driver=DriverName
  },
  SeedSpec = #{
    id => errm_seed,
    start => {
      errm_seed,
      start_link,
      [SeedName, SeedInit]
    },
    restart => permanent,
    shutdown => default_shutdown(SeedShutdown),
    type => worker,
    modules => [errm_seed, errm_changeset]
  },

  ChildSpecs = [DriverSpec, MigrateSpec, SeedSpec],
  {ok, {SupFlags, ChildSpecs}}.

% internal functions

-spec default_shutdown(N :: pos_integer() | undefined) -> pos_integer().
default_shutdown(undefined) ->
  ?ERRM_REPO_DEFAULT_SHUTDOWN;
default_shutdown(N) when is_integer(N), N > 0 ->
  N.

-spec default_pool_min(N :: pos_integer() | undefined) -> pos_integer().
default_pool_min(undefined) ->
  ?ERRM_REPO_DEFAULT_POOL_MIN;
default_pool_min(N) when is_integer(N), N > 0 ->
  N.

-spec default_pool_max(N :: pos_integer() | undefined) -> pos_integer().
default_pool_max(undefined) ->
  ?ERRM_REPO_DEFAULT_POOL_MAX;
default_pool_max(N) when is_integer(N), N > 0 ->
  N.

-spec default_replica_enabled(Bool :: boolean() | undefined) -> boolean().
default_replica_enabled(undefined) ->
  ?ERRM_REPO_DEFAULT_REPLICA_ENABLED;
default_replica_enabled(Bool) when is_boolean(Bool) ->
  Bool.