-module(errm_repo).

-include("errm_repo.hrl").
-include("errm_utils.hrl").
-include("errm_error.hrl").

-behavior(gen_server).

%% API
-export([
  start_link/1,
  start_link/2,
  stop/0,
  stop/1,
  get_repo/0,
  get_repo/1,
  get_driver_server/0,
  get_driver_server/1,
  get_migration_server/0,
  get_migration_server/1,
  get_seeds_server/0,
  get_seeds_server/1
]).
-export([
  init/1,
  terminate/2,
  handle_call/3,
  handle_cast/2,
  handle_info/2
]).

% @doc Start and link default repository server.
% @param RepoInit   Initialization parameters for starting wrapped supervisor of driver, migration, and seed server.
% @returns `{ok, errm_server_ref()} | {error, term()}'
% @equiv start_link(errm_repo_default, RepoInit)
-spec start_link(RepoInit :: errm_repo_init()) -> {ok, errm_server_ref()} | {error, term()}.
start_link(RepoInit) ->
  start_link(?ERRM_REPO_DEFAULT, RepoInit).

% @doc Start and link repository server with name `Name'.
% @param Name       Name of repository server.
% @param RepoInit   Initialization parameters for starting wrapped supervisor of driver, migration, and seed server.
% @returns `{ok, errm_server_ref()} | {error, term()}'
-spec start_link(Name :: atom(), RepoInit :: errm_repo_init()) -> {ok, errm_server_ref()} | {error, term()}.
start_link(Name, RepoInit) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, RepoInit], []).

% @doc Stop default repository server.
% @returns `ok'
% @equiv stop(errm_repo_default)
-spec stop() -> ok.
stop() ->
  stop(?ERRM_REPO_DEFAULT).

% @doc Stop repository server with name `Name'.
% @param Name   Name of repository server.
% @returns `ok'
-spec stop(Name :: errm_server_ref()) -> ok.
stop(Name) ->
  gen_server:stop(Name).

% @doc Get record of repository information (driver server name, migration server name, seed server name) for default repository.
% @returns `{ok, errm_repo_server()} | {error, errm_error()}'
% @equiv get_repo(errm_repo_default)
-spec get_repo() -> {ok, errm_repo_server()} | {error, errm_error()}.
get_repo() ->
  get_repo(?ERRM_REPO_DEFAULT).

% @doc Get record of repository information (driver server name, migration server name, seed server name) for repository
% server `Name'.
% @param Name   Name of repository server.
% @returns `{ok, errm_repo_server()} | {error, errm_error()}'
-spec get_repo(Name :: errm_server_ref()) -> {ok, errm_repo_server()} | {error, errm_error()}.
get_repo(Name) ->
  [Repo] = ets:lookup(?ERRM_REPO_DEFAULT_DRIVER, Name),
  {ok, Repo}.

% @doc Get name of driver server for default repository.
% @returns `{ok, errm_server_ref()} | {error, errm_error()}'
% @equiv get_driver_server(errm_repo_default)
-spec get_driver_server() -> {ok, errm_server_ref()} | {error, errm_error()}.
get_driver_server() ->
  get_driver_server(?ERRM_REPO_DEFAULT).

% @doc Get name of driver server for repository server with name `Name'.
% @param Name   Name of repository server.
% @returns `{ok, errm_server_ref()} | {error, errm_error()}'
-spec get_driver_server(Name :: errm_server_ref()) -> {ok, errm_server_ref()} | {error, errm_error()}.
get_driver_server(Name) ->
  case ets:lookup(?ERRM_REPO_TABLE, Name) of
    [#errm_repo_server{ name=Name, driver=Driver }] ->
      {ok, Driver};
    _ ->
      {error, errm_error:new(?ERRM_NO_REPO_ERROR, "Repository not found")}
  end.

% @doc Get name of migration server for default repository.
% @returns `{ok, errm_server_ref()} | {error, errm_error()}'
% @equiv get_migration_server(errm_repo_default)
-spec get_migration_server() -> {ok, errm_server_ref()} | {error, errm_error()}.
get_migration_server() ->
  get_migration_server(?ERRM_REPO_DEFAULT).

% @doc Get name of migration server for repository server with name `Name'.
% @param Name   Name of repository server.
% @returns `{ok, errm_server_ref()} | {error, errm_error()}'
-spec get_migration_server(Name :: errm_server_ref()) -> {ok, errm_server_ref()} | {error, errm_error()}.
get_migration_server(Name) ->
  case ets:lookup(?ERRM_REPO_TABLE, Name) of
    [#errm_repo_server{ name=Name, migrations=Migrations }] ->
      {ok, Migrations};
    _ ->
      {error, errm_error:new(?ERRM_NO_REPO_ERROR, "Repository not found")}
  end.

% @doc Get name of seed server for default repository.
% @returns `{ok, errm_server_ref()} | {error, errm_error()}'
% @equiv get_seed_server(errm_repo_default)
-spec get_seeds_server() -> {ok, errm_server_ref()} | {error, errm_error()}.
get_seeds_server() ->
  get_seeds_server(?ERRM_REPO_DEFAULT).

% @doc Get name of seed server for repository server with name `Name'.
% @param Name   Name of repository server.
% @returns `{ok, errm_server_ref()} | {error, errm_error()}'
-spec get_seeds_server(Name :: errm_server_ref()) -> {ok, errm_server_ref()} | {error, errm_error()}.
get_seeds_server(Name) ->
  case ets:lookup(?ERRM_REPO_TABLE, Name) of
    [#errm_repo_server{ name=Name, seeds=Seeds }] ->
      {ok, Seeds};
    _ ->
      {error, errm_error:new(?ERRM_NO_REPO_ERROR, "Repository not found")}
  end.

% gen_server callbacks

init([
  Name,
  RepoInit=#errm_repo_init{
    driver_server_name=DriverName,
    migration_server_name=MigrationName,
    seed_server_name=SeedName
  }
]) ->
  {ok, Sup} = errm_repo_sup:start_link(RepoInit),
  true = ets:insert(
    ?ERRM_REPO_TABLE,
    #errm_repo_server{ name=Name, driver=DriverName, migrations=MigrationName, seeds=SeedName }
  ),
  {ok, #errm_repo_state{ name=Name, table=?ERRM_REPO_TABLE, sup=Sup }}.

terminate(_Reason, #errm_repo_state{ name=Name, table=Table }) ->
  true = ets:delete(Table, Name),
  ok.

handle_call(_Req, _From, State) ->
  {noreply, State}.

handle_cast(_Cast, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.