-module(errm_repo_init).

-include("errm_repo.hrl").

%% public methods
-export([
  from_map/1
]).

% public methods

-spec from_map(
  Map :: #{
    repo_supervisor_intensity => pos_integer() | undefined,
    repo_supervisor_period => pos_integer() | undefined,
    driver_server_name := atom(),
    driver_module := module(),
    driver_args := term(),
    driver_pool_min => pos_integer() | undefined,
    driver_pool_max => pos_integer() | undefined,
    replica_enabled => boolean() | undefined,
    replica_args => term() | undefined,
    replica_pool_min => pos_integer() | undefined,
    replica_pool_max => pos_integer() | undefined,
    migration_server_name := atom(),
    migration_table := string(),
    migration_schema := string(),
    migration_path := string(),
    migration_shutdown => pos_integer() | undefined,
    seed_server_name := atom(),
    seed_table := string(),
    seed_schema := string(),
    seed_path := string(),
    seed_shutdown => pos_integer() | undefined
  }
) -> errm_repo_init().
from_map(Map) when is_map(Map) ->
  {SupIntensity, SupPeriod} = build_supervisor(Map),

  {
    DriverName,
    DriverMod,
    DriverArgs,
    DriverPoolMin,
    DriverPoolMax,
    DriverShutdown,
    ReplicaEnabled,
    ReplicaArgs,
    ReplicaPoolMin,
    ReplicaPoolMax
  } = build_driver(Map),

  {MigrationName, MigrationTable, MigrationSchema, MigrationPath, MigrationShutdown} = build_migration(Map),

  {SeedName, SeedTable, SeedSchema, SeedPath, SeedShutdown} = build_seed(Map),

  #errm_repo_init{
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
  }.

% internal functions

build_supervisor(Map) ->
  SupIntensity = maps:get(repo_supervisor_intensity, Map, ?ERRM_REPO_DEFAULT_SUPERVISOR_INTENSITY),
  SupPeriod = maps:get(repo_supervisor_period, Map, ?ERRM_REPO_DEFAULT_SUPERVISOR_INTENSITY),

  true = validate_values([
    {fun is_pos_integer/1, SupIntensity},
    {fun is_pos_integer/1, SupPeriod}
  ]),

  {SupIntensity, SupPeriod}.

build_driver(Map) ->
  DriverName = maps:get(driver_server_name, Map),
  DriverMod = maps:get(driver_module, Map),
  DriverArgs = maps:get(driver_args, Map),
  DriverPoolMin = maps:get(driver_pool_min, Map, ?ERRM_REPO_DEFAULT_POOL_MIN),
  DriverPoolMax = maps:get(driver_pool_max, Map, ?ERRM_REPO_DEFAULT_POOL_MAX),
  DriverShutdown = maps:get(driver_shutdown, Map, ?ERRM_REPO_DEFAULT_SHUTDOWN),
  ReplicaEnabled = maps:get(replica_enabled, Map, ?ERRM_REPO_DEFAULT_REPLICA_ENABLED),

  {RArgs, RPoolMin, RPoolMax} = case ReplicaEnabled of
                                  true ->
                                    Args = maps:get(replicla_args, Map),
                                    PoolMin = maps:get(replica_pool_min, Map, ?ERRM_REPO_DEFAULT_POOL_MIN),
                                    PoolMax = maps:get(replica_pool_max, Map, ?ERRM_REPO_DEFAULT_POOL_MAX),
                                    {Args, PoolMin, PoolMax};
                                  false ->
                                    {undefined, undefined, undefined}
                                end,

  validate_values([
    {fun is_atom/1, DriverName},
    {fun is_atom/1, DriverMod},
    {fun is_pos_integer/1, DriverPoolMin},
    {fun is_pos_integer/1, DriverPoolMax},
    {fun is_pos_integer/1, DriverShutdown},
    {fun is_boolean/1, ReplicaEnabled},
    {fun is_pos_integer_or_undefined/1, RPoolMin},
    {fun is_pos_integer_or_undefined/1, RPoolMax}
  ]),

  {
    DriverName,
    DriverMod,
    DriverArgs,
    DriverPoolMin,
    DriverPoolMax,
    DriverShutdown,
    ReplicaEnabled,
    RArgs,
    RPoolMin,
    RPoolMax
  }.

build_migration(Map) ->
  MigrationName = maps:get(migration_server_name, Map),
  MigrationTable = maps:get(migration_table, Map),
  MigrationSchema = maps:get(migration_schema, Map),
  MigrationPath = maps:get(migration_schema, Map),
  MigrationShutdown = maps:get(migration_shutdown, Map, ?ERRM_REPO_DEFAULT_SHUTDOWN),

  validate_values([
    {fun is_atom/1, MigrationName},
    {fun is_list/1, MigrationTable},
    {fun is_list/1, MigrationSchema},
    {fun is_list/1, MigrationPath},
    {fun is_pos_integer/1, MigrationShutdown}
  ]),

  {MigrationName, MigrationTable, MigrationSchema, MigrationPath, MigrationShutdown}.

build_seed(Map) ->
  SeedName = maps:get(seed_server_name, Map),
  SeedTable = maps:get(seed_table, Map),
  SeedSchema = maps:get(seed_schema, Map),
  SeedPath = maps:get(seed_schema, Map),
  SeedShutdown = maps:get(seed_shutdown, Map, ?ERRM_REPO_DEFAULT_SHUTDOWN),

  validate_values([
    {fun is_atom/1, SeedName},
    {fun is_list/1, SeedTable},
    {fun is_list/1, SeedSchema},
    {fun is_list/1, SeedPath},
    {fun is_pos_integer/1, SeedShutdown}
  ]),

  {SeedName, SeedTable, SeedSchema, SeedPath, SeedShutdown}.

is_pos_integer(N) when is_integer(N), N > 0 ->
  true;
is_pos_integer(_) ->
  false.

is_pos_integer_or_undefined(N) when is_integer(N), N > 0 ->
  true;
is_pos_integer_or_undefined(undefined) ->
  true;
is_pos_integer_or_undefined(_) ->
  false.

validate_values([]) ->
  true;
validate_values([{Fn, Val} | Rest]) ->
  case Fn(Val) of
    true -> validate_values(Rest);
    false -> false
  end.