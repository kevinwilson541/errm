%%%-------------------------------------------------------------------
%% @doc errm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(errm_sup).

-include("errm_config.hrl").
-include("errm_repo.hrl").

-behaviour(supervisor).

-export([
  start_link/0,
  start_repo/2,
  stop_repo/1
]).

-export([
  init/1
]).

-define(SERVER, ?MODULE).

% @doc Start and link errm supervisor.
% @returns `{ok pid()} | {error, term()}'
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% @doc Start repository server with name `Name' and initialization parameters `Init' under errm supervisor.
% @param Name   Name of repository server to supervise.
% @param Init   Initialization parameters of repository server to supervise.
% @returns `{ok, pid()} | {error, term()}'
-spec start_repo(Name :: atom(), Init :: errm_repo_init()) -> {ok, pid()} | {error, term()}.
start_repo(Name, Init) ->
  ChildSpec = #{
    id => Name,
    start => {errm_repo, start_link, [Name, Init]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [errm_repo]
  },
  supervisor:start_child(?SERVER, ChildSpec).

% @doc Stop supervised repository server with name `Name'.
% @param Name   Name of repository server to terminate.
% @returns `ok'
-spec stop_repo(Name :: atom()) -> ok.
stop_repo(Name) ->
  ok = supervisor:terminate_child(?SERVER, Name),
  ok = supervisor:delete_child(?SERVER, Name),
  ok.

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  DefaultRepoEnabled = application:get_env(errm, ?ERRM_DEFAULT_REPO_ENABLED, ?ERRM_REPO_DEFAULT_DRIVER_ENABLED),
  RepoSpec = case DefaultRepoEnabled of
               false -> [];
               true -> define_default_repo()
             end,

  SupFlags = #{
    strategy => one_for_one,
    intensity => 1,
    period => 5000
  },

  ets:new(?ERRM_REPO_TABLE, [named_table, public, {read_concurrency, true}, {keypos, 2}]),

  {ok, {SupFlags, [RepoSpec]}}.

%% internal functions

-spec define_default_repo() -> supervisor:child_spec().
define_default_repo() ->
  {ok, SupervisorIntensity} = application:get_env(errm, ?ERRM_SUPERVISOR_INTENSITY),
  {ok, SupervisorPeriod} = application:get_env(errm, ?ERRM_SUPERVISOR_PERIOD),

  % driver config
  {ok, DriverModule} = application:get_env(errm, ?ERRM_DRIVER_MODULE),
  {ok, DriverArgs} = application:get_env(errm, ?ERRM_DRIVER_INIT),
  DriverPoolMin = application:get_env(errm, ?ERRM_DRIVER_POOL_MIN, ?ERRM_REPO_DEFAULT_POOL_MIN),
  DriverPoolMax = application:get_env(errm, ?ERRM_DRIVER_POOL_MAX, ?ERRM_REPO_DEFAULT_POOL_MAX),
  DriverShutdown = application:get_env(errm, ?ERRM_DRIVER_SHUTDOWN, ?ERRM_REPO_DEFAULT_SHUTDOWN),

  % replica config
  ReplicaEnabled = application:get_env(errm, ?ERRM_DRIVER_REPLICA_ENABLED, ?ERRM_REPO_DEFAULT_REPLICA_ENABLED),
  {ReplicaArgs, ReplicaPoolMin, ReplicaPoolMax} = case ReplicaEnabled of
                                                    false -> {undefined, undefined, undefined};
                                                    true ->
                                                      define_default_replica()
                                                  end,

  % migrations config
  {ok, MigrationTable} = application:get_env(errm, ?ERRM_MIGRATION_TABLE),
  {ok, MigrationSchema} = application:get_env(errm, ?ERRM_MIGRATION_SCHEMA),
  {ok, MigrationPath} = application:get_env(errm, ?ERRM_MIGRATION_PATH),
  MigrationShutdown = application:get_env(errm, ?ERRM_MIGRATION_SHUTDOWN, ?ERRM_REPO_DEFAULT_SHUTDOWN),

  % seeds config
  {ok, SeedTable} = application:get_env(errm, ?ERRM_SEED_TABLE),
  {ok, SeedSchema} = application:get_env(errm, ?ERRM_SEED_SCHEMA),
  {ok, SeedPath} = application:get_env(errm, ?ERRM_SEED_PATH),
  SeedShutdown = application:get_env(errm, ?ERRM_SEED_SHUTDOWN, ?ERRM_REPO_DEFAULT_SHUTDOWN),

  RepoInit = #errm_repo_init{
    repo_supervisor_intensity=SupervisorIntensity,
    repo_supervisor_period=SupervisorPeriod,
    driver_server_name=?ERRM_REPO_DEFAULT_DRIVER,
    driver_module=DriverModule,
    driver_args=DriverArgs,
    driver_pool_min=DriverPoolMin,
    driver_pool_max=DriverPoolMax,
    replica_enabled=ReplicaEnabled,
    replica_args=ReplicaArgs,
    replica_pool_min=ReplicaPoolMin,
    replica_pool_max=ReplicaPoolMax,
    driver_shutdown=DriverShutdown,
    migration_server_name=?ERRM_REPO_DEFAULT_MIGRATE,
    migration_table=MigrationTable,
    migration_schema=MigrationSchema,
    migration_path=MigrationPath,
    migration_shutdown=MigrationShutdown,
    seed_server_name=?ERRM_REPO_DEFAULT_SEED,
    seed_table=SeedTable,
    seed_schema=SeedSchema,
    seed_path=SeedPath,
    seed_shutdown=SeedShutdown
  },

  RepoSpec = #{
    id => ?ERRM_REPO_DEFAULT,
    start => {errm_repo, start_link, [?ERRM_REPO_DEFAULT, RepoInit]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [errm_repo]
  },

  RepoSpec.

-spec define_default_replica() -> {[term()], pos_integer(), pos_integer()}.
define_default_replica() ->
  {ok, ReplicaArgs} = application:get_env(errm, ?ERRM_DRIVER_REPLICA_INIT),
  ReplicaPoolMin = application:get_env(errm, ?ERRM_DRIVER_REPLICA_POOL_MIN, ?ERRM_REPO_DEFAULT_POOL_MIN),
  ReplicaPoolMax = application:get_env(errm, ?ERRM_DRIVER_REPLICA_POOL_MAX, ?ERRM_REPO_DEFAULT_POOL_MAX),

  {ReplicaArgs, ReplicaPoolMin, ReplicaPoolMax}.