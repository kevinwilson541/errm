% @doc Behavior module for defining a database driver, to be managed by a wrapping connection server with module
% `errm_connection'. For testing purposes, an example sqlite3 driver is implemented in the `test' directory.
%
% `start_link/1': Start the driver and link it to the managing connection server.
%
% `stop/1': Stop the driver.
%
% `query/4': Run a query against the driver.
%
% `exec/3': Execute a statement against the driver.
%
% `start_transaction/3': Start a transaction against the driver.
%
% `commit_transaction/2': Commit a transaction against the driver.
%
% `rollback_transaction/2': Rollback a transaction against the driver.
%
% `savepoint/3': Create a savepoint against the driver.
%
% `release_savepoint/3': Release a savepoint against the driver.
%
% `revert_savepoint/3': Revert a savepoint against the driver.
%
% @see errm_connection

-module(errm_gen_driver).

-include("errm_error.hrl").
-include("errm_gen_driver.hrl").

%% API
-export([]).

-callback start_link(Init :: term()) -> {ok, term()}.

-callback stop(Connection :: term()) -> ok.

-callback query(
  Connection :: term(),
  Query :: iodata(),
  Parameters :: list(),
  Timeout :: pos_integer()
) -> {ok, errm_query_reply()} | {error, errm_error()}.

-callback exec(
  Connection :: term(),
  Query :: iodata(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.

-callback start_transaction(
  Connection :: term(),
  IsolationLevel :: iodata(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.

-callback commit_transaction(
  Connection :: term(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.

-callback rollback_transaction(
  Connection :: term(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.

-callback savepoint(
  Connection :: term(),
  Name :: iodata(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.

-callback release_savepoint(
  Connection :: term(),
  Name :: iodata(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.

-callback revert_savepoint(
  Connection :: term(),
  Name :: iodata(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.