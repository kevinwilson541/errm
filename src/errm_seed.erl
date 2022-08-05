% @doc A `gen_server' module for managing a set of seeds to be run against a database driver managed by `errm_driver'.
%
% Seeds are read from a directory path containing a set of YAML files of the following structure:
%
% ```
%   up:
%     sql: sql to run to add seed to the database
%     transaction: whether to run the seed in a database transaction
%     timeout: Number of milliseconds to wait for the seed to run before returning an error
%   down:
%     sql: sql to run to remove the seed from the database
%     transaction: whether to run the seed in a database transaction
%     timeout: Number of milliseconds to wait for the seed to run before returning an error
% '''
%
% The seeds found in this directory will have a run-order based on erlang string sorting of the file name (without YAML
% extension), which we'll refer to as ID from now on.
%
% NOTE: it's suggested to use a date or fixed-length numerical prefix on file names within seed directory (e.g.
% unix timestamp, ISO-8604 string, etc., followed by human-readable description of seed).
%
% To prevent rerunning seeds against a database when already added/removed, the name of seeds will be stored in
% a schema and table combination provided by the application user with the following definition:
%
% ```
%   CREATE TABLE example_schema.example_table (
%     name TEXT PRIMARY KEY
%   )
% '''
%
% When a seed is added to the database, the corresponding ID will be inserted into this table; when a seed is removed
% from the database, the corresponding ID will be removed from this table. For seeds run within a transaction, this
% operation will be encompassed within the database transaction s.t. the seed and addition/removal from recording table
% are ACID compliant.
%
% NOTE: as a note on best practice, seeds should be written as idempotent functions when possible (on both addition and
% removal).
%
% Functions are provided in order to add all seeds from a seed set to a database, remove all seeds from a seed set
% to a database, add all seeds up to and including a given ID from the seed set, and remove all seeds down to and
% including a given ID from the seed set. Each seed in these calls will run sequentially. If any seed from a seed set
% should fail to run (e.g. timeout, transaction failure, driver error), any pending seeds in the seed set will not run,
% and the error will be returned to the caller.
%
% Under the hood, this module wraps the `errm_changeset' module functionality (as errm_changeset is a generic gen_server
% for managing the addition and removal of changes to a database, both schema based and content based).
%
% @see errm_changeset

-module(errm_seed).

-include("errm_seed.hrl").
-include("errm_changeset.hrl").
-include("errm_error.hrl").
-include("errm_utils.hrl").

%% API
-export([
  start_link/1,
  start_link/2,
  stop/1,
  up/1,
  up/2,
  up/3,
  down/1,
  down/2,
  down/3,
  status/1
]).

%% public methods

% @doc Starts and links seed server with provided initialization parameters.
% @param Init   Initialization record containing file path, table, schema, and driver pid.
% @returns `{ok, pid()} | {error, term()}'
% @equiv start_link(errm_seed, Init)
-spec start_link(Init :: errm_seed_init()) -> {ok, pid()}.
start_link(Init=#errm_seed_init{}) ->
  start_link(?MODULE, Init).

% @doc Starts and links seed server with provided name and initialization parameters.
% @param Name   Name to register seed server with.
% @param Init   Initialization record containing file path, table, schema, and driver pid.
% @returns `{ok, pid()} | {error, term()}'
% @see errm_changeset:start_link/2
-spec start_link(Name :: atom(), Init :: errm_seed_init()) -> {ok, pid()}.
start_link(Name, #errm_seed_init{ path=Path, table_name=Table, schema=Schema, driver=Driver }) ->
  errm_changeset:start_link(Name, #errm_changeset_init{ path=Path, table_name=Table, schema=Schema, driver=Driver }).

% @doc Stops seed server.
% @param Pid    Pid or registered name of seed server.
% @returns `ok'
% @see errm_changeset:stop/1
-spec stop(Pid :: errm_server_ref()) -> ok.
stop(Pid) ->
  errm_changeset:stop(Pid).

% @doc Adds all seeds to the database.
% @param Pid    Pid or name of seed server.
% @returns `ok | {error, errm_error()}'
% @equiv up(Pid, all)
% @see errm_changeset:up/1
-spec up(Pid :: errm_server_ref()) -> ok | {error, errm_error()}.
up(Pid) ->
  errm_changeset:up(Pid).

% @doc Adds all seeds up to and including the seed with ID `Id'.
% @param Pid    Pid or name of seed server.
% @param Id     ID of seed to add seeds up to (inclusive).
% @returns `ok | {error, errm_error()}'
% @equiv up(Pid, Id, 120000)
% @see errm_changeset:up/2
-spec up(Pid :: errm_server_ref(), Id :: string() | all) -> ok | {error, errm_error()}.
up(Pid, Id) ->
  errm_changeset:up(Pid, Id).

% @doc Adds all seeds up to and including the seed with ID `Id', timing out after `Timeout' milliseconds.
% @param Pid      Pid or name of seed server.
% @param Id       ID of seed to add seeds up to (inclusive).
% @param Timeout  Number of milliseconds to wait for seeds to be added before timing out.
% @returns `ok | {error, errm_error()}'
% @see errm_changeset:up/3
-spec up(Pid :: errm_server_ref(), Id :: string() | all, Timeout :: pos_integer()) -> ok | {error, errm_error()}.
up(Pid, Id, Timeout) ->
  errm_changeset:up(Pid, Id, Timeout).

% @doc Removes all seeds from the database.
% @param Pid    Pid or name of seed server.
% @returns `ok | {error, errm_error()}'
% @equiv down(Pid, all)
% @see errm_changeset:down/1
-spec down(Pid :: errm_server_ref()) -> ok | {error, errm_error()}.
down(Pid) ->
  errm_changeset:down(Pid).

% @doc Removes all seeds down to and including the seed with ID `Id'.
% @param Pid    Pid or name of seed server.
% @param Id     ID of seed to remove seeds down to (inclusive).
% @returns `ok | {error, errm_error()}'
% @equiv down(Pid, Id, 120000)
% @see errm_changeset:down/2
-spec down(Pid :: errm_server_ref(), Id :: string() | all) -> ok | {error, errm_error()}.
down(Pid, Id) ->
  errm_changeset:down(Pid, Id).

% @doc Removes all seeds down to and including the seed with ID `Id', timing out after `Timeout' milliseconds.
% @param Pid      Pid or name of seed server.
% @param Id       ID of seed to remove seeds down to (inclusive).
% @param Timeout  Number of milliseconds to wait for seeds to be removed before timing out.
% @returns `ok | {error, errm_error()}'
% @see errm_changeset:down/3
-spec down(Pid :: errm_server_ref(), Id :: string() | all, Timeout :: pos_integer()) -> ok | {error, errm_error()}.
down(Pid, Id, Timeout) ->
  errm_changeset:down(Pid, Id, Timeout).

% @doc Get local status of seeds.
% @param Pid    Pid or name of seed server.
% @returns `{ok, [errm_changeset()]} | {error, errm_error()}'
% @see errm_changeset:status/1
-spec status(Pid :: errm_server_ref()) -> {ok, [errm_changeset()]} | {error, errm_error()}.
status(Pid) ->
  errm_changeset:status(Pid).