-module(errm).

-include("errm_repo.hrl").
-include("errm_utils.hrl").
-include("errm_error.hrl").
-include("errm_connection.hrl").
-include("errm_driver.hrl").
-include("errm_gen_driver.hrl").

%% API
-export([
  default_repo/0,
  start_repo/2,
  stop_repo/1,
  get_connection/1,
  get_connection/2,
  get_connection/3,
  release_connection/2,
  release_connection/3,
  with_connection/2,
  with_connection/3,
  with_connection/4,
  start_transaction/2,
  start_transaction/3,
  rollback_transaction/2,
  rollback_transaction/3,
  transaction/3,
  transaction/4,
  query/2,
  query/3,
  query/4,
  exec/2,
  exec/3,
  exec/4,
  migrations_up/1,
  migrations_up/2,
  migrations_up/3,
  migrations_down/1,
  migrations_down/2,
  migrations_down/3,
  seeds_up/1,
  seeds_up/2,
  seeds_up/3,
  seeds_down/1,
  seeds_down/2,
  seeds_down/3
]).

% public methods

-spec default_repo() -> errm_server_ref().
default_repo() ->
  ?ERRM_REPO_DEFAULT.

-spec start_repo(Name :: atom(), RepoInit :: errm_repo_init()) -> {ok, pid()} | {error, term()}.
start_repo(Name, RepoInit) ->
  errm_sup:start_repo(Name, RepoInit).

-spec stop_repo(Name :: errm_server_ref()) -> ok.
stop_repo(Name) ->
  errm_sup:stop_repo(Name).

-spec get_connection(Repo :: errm_server_ref()) -> {ok, errm_connection()} | {error, errm_error()}.
get_connection(Repo) ->
  case get_driver(Repo) of
    {ok, Driver} -> errm_driver:get_connection(Driver);
    {error, Err} -> {error, Err}
  end.

-spec get_connection(
  Repo :: errm_server_ref(),
  ConnType :: errm_connection_type()
) -> {ok, errm_connection()} | {error, errm_error()}.
get_connection(Repo, ConnType) ->
  case get_driver(Repo) of
    {ok, Driver} -> errm_driver:get_connection(Driver, ConnType);
    {error, Err} -> {error, Err}
  end.

-spec get_connection(
  Repo :: errm_server_ref(),
  ConnType :: errm_connection_type(),
  Timeout :: pos_integer()
) -> {ok, errm_connection()} | {error, errm_error()}.
get_connection(Repo, ConnType, Timeout) ->
  case get_driver(Repo) of
    {ok, Driver} -> errm_driver:get_connection(Driver, ConnType, Timeout);
    {error, Err} -> {error, Err}
  end.

-spec release_connection(
  Repo :: errm_server_ref(),
  ConnType :: errm_connection()
) -> ok | {error, errm_error()}.
release_connection(Repo, Conn) ->
  case get_driver(Repo) of
    {ok, Driver} -> errm_driver:release_connection(Driver, Conn);
    {error, Err} -> {error, Err}
  end.

-spec release_connection(
  Repo :: errm_server_ref(),
  ConnType :: errm_connection(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.
release_connection(Repo, Conn, Timeout) ->
  case get_driver(Repo) of
    {ok, Driver} -> errm_driver:release_connection(Driver, Conn, Timeout);
    {error, Err} -> {error, Err}
  end.

-spec with_connection(
  Repo :: errm_server_ref(),
  Fn :: fun((Conn :: errm_connection()) -> {ok, term()} | {error, term()})
) -> {ok, term()} | {error, term()}.
with_connection(Repo, Fn) ->
  case get_driver(Repo) of
    {ok, Driver} ->
      case errm_driver:get_connection(Driver) of
        {ok, Conn} -> try_run_on_connection(Driver, Conn, Fn);
        {error, Err} -> {error, Err}
      end;
    {error, Err} ->
      {error, Err}
  end.

-spec with_connection(
  Repo :: errm_server_ref(),
  ConnType :: errm_connection_type(),
  Fn :: fun((Conn :: errm_connection()) -> {ok, term()} | {error, term()})
) -> {ok, term()} | {error, term()}.
with_connection(Repo, ConnType, Fn) ->
  case get_driver(Repo) of
    {ok, Driver} ->
      case errm_driver:get_connection(Driver, ConnType) of
        {ok, Conn} -> try_run_on_connection(Driver, Conn, Fn);
        {error, Err} -> {error, Err}
      end;
    {error, Err} ->
      {error, Err}
  end.

-spec with_connection(
  Repo :: errm_server_ref(),
  ConnType :: errm_connection_type(),
  Timeout :: pos_integer(),
  Fn :: fun((Conn :: errm_connection()) -> {ok, term()} | {error, term()})
) -> {ok, term()} | {error, term()}.
with_connection(Repo, ConnType, Timeout, Fn) ->
  case get_driver(Repo) of
    {ok, Driver} ->
      case errm_driver:get_connection(Driver, ConnType, Timeout) of
        {ok, Conn} -> try_run_on_connection(Driver, Conn, Fn);
        {error, Err} -> {error, Err}
      end;
    {error, Err} ->
      {error, Err}
  end.

-spec start_transaction(
  Conn :: errm_connection(),
  IsolationLevel :: iodata()
) -> {ok, errm_transaction()} | {error, errm_error()}.
start_transaction(Conn, IsolationLevel) ->
  errm_connection:start_transaction(Conn, IsolationLevel).

-spec start_transaction(
  Conn :: errm_connection(),
  IsolationLevel :: iodata(),
  Timeout :: pos_integer()
) -> {ok, errm_transaction()} | {error, errm_error()}.
start_transaction(Conn, IsolationLevel, Timeout) ->
  errm_connection:start_transaction(Conn, IsolationLevel, Timeout).

-spec rollback_transaction(
  Conn :: errm_connection(),
  TxRef :: errm_transaction()
) -> ok | {error, errm_error()}.
rollback_transaction(Conn, TxRef) ->
  errm_connection:rollback_transaction(Conn, TxRef).

-spec rollback_transaction(
  Conn :: errm_connection(),
  TxRef :: errm_transaction(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.
rollback_transaction(Conn, TxRef, Timeout) ->
  errm_connection:rollback_transaction(Conn, TxRef, Timeout).

-spec transaction(
  Conn :: errm_connection(),
  IsolationLevel :: iodata(),
  Fn :: fun((Conn :: errm_connection(), TxRef :: errm_transaction()) -> {ok, term()} | {error, errm_error()})
) -> {ok, term()} | {error, errm_error()}.
transaction(Conn, IsolationLevel, Fn) ->
  errm_connection:transaction(Conn, IsolationLevel, Fn).

-spec transaction(
  Conn :: errm_connection(),
  IsolationLevel :: iodata(),
  Timeout :: pos_integer(),
  Fn :: fun((Conn :: errm_connection(), TxRef :: errm_transaction()) -> {ok, term()} | {error, errm_error()})
) -> {ok, term()} | {error, errm_error()}.
transaction(Conn, IsolationLevel, Timeout, Fn) ->
  errm_connection:transaction(Conn, IsolationLevel, Timeout, Fn).

-spec query(Conn :: errm_connection(), Query :: iodata()) -> {ok, errm_query_reply()} | {error, errm_error()}.
query(Conn, Query) ->
  errm_connection:query(Conn, Query).

-spec query(
  Conn :: errm_connection(),
  Query :: iodata(),
  Opts :: errm_query_options()
) -> {ok, errm_query_reply()} | {error, errm_error()}.
query(Conn, Query, Opts) ->
  errm_connection:query(Conn, Query, Opts).

-spec query(
  Conn :: errm_connection(),
  Query :: iodata(),
  Opts :: errm_query_options(),
  Timeout :: pos_integer()
) -> {ok, errm_query_reply()} | {error, errm_error()}.
query(Conn, Query, Opts, Timeout) ->
  errm_connection:query(Conn, Query, Opts, Timeout).

-spec exec(Conn :: errm_connection(), Query :: iodata()) -> ok | {error, errm_error()}.
exec(Conn, Query) ->
  errm_connection:exec(Conn, Query).

-spec exec(
  Conn :: errm_connection(),
  Query :: iodata(),
  Opts :: errm_exec_options()
) -> ok | {error, errm_error()}.
exec(Conn, Query, Opts) ->
  errm_connection:exec(Conn, Query, Opts).

-spec exec(
  Conn :: errm_connection(),
  Query :: iodata(),
  Opts :: errm_exec_options(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.
exec(Conn, Query, Opts, Timeout) ->
  errm_connection:exec(Conn, Query, Opts, Timeout).

-spec migrations_up(Repo :: errm_server_ref()) -> ok | {error, errm_error()}.
migrations_up(Repo) ->
  case get_migrations(Repo) of
    {ok, Migrations} -> errm_migrate:up(Migrations);
    {error, Err} -> {error, Err}
  end.

-spec migrations_up(Repo :: errm_server_ref(), Id :: string() | all) -> ok | {error, errm_error()}.
migrations_up(Repo, Id) ->
  case get_migrations(Repo) of
    {ok, Migrations} -> errm_migrate:up(Migrations, Id);
    {error, Err} -> {error, Err}
  end.

-spec migrations_up(
  Repo :: errm_server_ref(),
  Id :: string() | all,
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.
migrations_up(Repo, Id, Timeout) ->
  case get_migrations(Repo) of
    {ok, Migrations} -> errm_migrate:up(Migrations, Id, Timeout);
    {error, Err} -> {error, Err}
  end.

-spec migrations_down(Repo :: errm_server_ref()) -> ok | {error, errm_error()}.
migrations_down(Repo) ->
  case get_migrations(Repo) of
    {ok, Migrations} -> errm_migrate:down(Migrations);
    {error, Err} -> {error, Err}
  end.

-spec migrations_down(Repo :: errm_server_ref(), Id :: string() | all) -> ok | {error, errm_error()}.
migrations_down(Repo, Id) ->
  case get_migrations(Repo) of
    {ok, Migrations} -> errm_migrate:down(Migrations, Id);
    {error, Err} -> {error, Err}
  end.

-spec migrations_down(
  Repo :: errm_server_ref(),
  Id :: string() | all,
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.
migrations_down(Repo, Id, Timeout) ->
  case get_migrations(Repo) of
    {ok, Migrations} -> errm_migrate:down(Migrations, Id, Timeout);
    {error, Err} -> {error, Err}
  end.

-spec seeds_up(Repo :: errm_server_ref()) -> ok | {error, errm_error()}.
seeds_up(Repo) ->
  case get_seeds(Repo) of
    {ok, Seeds} -> errm_seed:up(Seeds);
    {error, Err} -> {error, Err}
  end.

-spec seeds_up(Repo :: errm_server_ref(), Id :: string() | all) -> ok | {error, errm_error()}.
seeds_up(Repo, Id) ->
  case get_seeds(Repo) of
    {ok, Seeds} -> errm_seed:up(Seeds, Id);
    {error, Err} -> {error, Err}
  end.

-spec seeds_up(Repo :: errm_server_ref(), Id :: string() | all, Timeout :: pos_integer()) -> ok | {error, errm_error()}.
seeds_up(Repo, Id, Timeout) ->
  case get_seeds(Repo) of
    {ok, Seeds} -> errm_seed:up(Seeds, Id, Timeout);
    {error, Err} -> {error, Err}
  end.

-spec seeds_down(Repo :: errm_server_ref()) -> ok | {error, errm_error()}.
seeds_down(Repo) ->
  case get_seeds(Repo) of
    {ok, Seeds} -> errm_seed:down(Seeds);
    {error, Err} -> {error, Err}
  end.

-spec seeds_down(Repo :: errm_server_ref(), Id :: string() | all) -> ok | {error, errm_error()}.
seeds_down(Repo, Id) ->
  case get_seeds(Repo) of
    {ok, Seeds} -> errm_seed:down(Seeds, Id);
    {error, Err} -> {error, Err}
  end.

-spec seeds_down(
  Repo :: errm_server_ref(),
  Id :: string() | all,
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.
seeds_down(Repo, Id, Timeout) ->
  case get_seeds(Repo) of
    {ok, Seeds} -> errm_seed:down(Seeds, Id, Timeout);
    {error, Err} -> {error, Err}
  end.

% internal functions

-spec get_driver(Repo :: errm_server_ref()) -> {ok, errm_server_ref()} | {error, errm_error()}.
get_driver(Repo) ->
  errm_repo:get_driver_server(Repo).

-spec get_migrations(Repo :: errm_server_ref()) -> {ok, errm_server_ref()} | {error, errm_error()}.
get_migrations(Repo) ->
  errm_repo:get_migration_server(Repo).

-spec get_seeds(Repo :: errm_server_ref()) -> {ok, errm_server_ref()} | {error, errm_error()}.
get_seeds(Repo) ->
  errm_repo:get_seeds_server(Repo).

try_run_on_connection(Driver, Conn, Fn) ->
  try
    Fn(Conn)
  after
    errm_driver:release_connection(Driver, Conn)
  end.