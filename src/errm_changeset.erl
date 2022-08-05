% @doc A `gen_server' module for managing a set of changes to be run against a database driver managed by `errm_driver'.
%
% Changes are read from a directory path containing a set of YAML files of the following structure:
%
% ```
%   up:
%     sql: sql to run to add change to the database
%     transaction: whether to run the change in a database transaction
%     timeout: Number of milliseconds to wait for change to run before returning an error
%   down:
%     sql: sql to run to remove the change from the database
%     transaction: whether to run the change in a database transaction
%     timeout: Number of milliseconds to wait for change to run before returning an error
% '''
%
% The changes found in this directory will have a run-order based on erlang string sorting of the file name (without YAML
% extension), which we'll refer to as ID from now on.
%
% NOTE: it's suggested to use a date or fixed-length numerical prefix on file names within changeset directory (e.g.
% unix timestamp, ISO-8604 string, etc., followed by human-readable description of change).
%
% To prevent rerunning changes against a database when already added/removed, the name of changes will be stored in a schema
% and table combination provided by the application user with the following definition:
%
% ```
%   CREATE TABLE example_schema.example_table (
%     name TEXT PRIMARY KEY
%   )
% '''
%
% When a change is added to the database, the corresponding ID will be inserted into this table; when a change is removed
% from the database, the corresponding ID will be removed from this table. For changes run within a transaction, this
% operation will be encompassed within the database transaction s.t. the change and addition/removal from recording table
% are ACID compliant.
%
% NOTE: as a note on best practice, changes should be written as idempotent functions when possible (on both addition and
% removal).
%
% Functions are provided in order to add all changes from a changeset to a database, remove all changes from a changeset
% to a database, add all changes up to and including a given ID from the changeset, and remove all changes down to and
% including a given ID from the changeset. Each change in these calls will run sequentially. If any change from a changeset
% should fail to run (e.g. timeout, transaction failure, driver error), any pending changes in the changeset will not run,
% and the error will be returned to the caller.

-module(errm_changeset).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("errm_changeset.hrl").
-include("errm_driver.hrl").
-include("errm_query.hrl").
-include("errm_error.hrl").
-include("errm_result.hrl").
-include("errm_connection.hrl").
-include("errm_utils.hrl").

-behavior(gen_server).

% API
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
% gen_server behavior
-export([
  init/1,
  terminate/2,
  handle_call/3,
  handle_cast/2,
  handle_info/2
]).

% public methods

% @doc Starts and links changeset server with provided initialization parameters.
% @param Init   Initialization record containing file path, table, schema, and driver pid.
% @returns `{ok, pid()} | {error, term()}'
% @equiv start_link(errm_changeset, Init)
-spec start_link(Init :: errm_changeset_init()) -> {ok, pid()} | {error, term()}.
start_link(Init = #errm_changeset_init{}) ->
  gen_server:start_link(?MODULE, Init, []).

% @doc Starts and links changeset server with provided name and initialization parameters.
% @param Name   Name to register changeset server with.
% @param Init   Initialization record containing file path, table, schema, and driver pid.
% @returns `{ok, pid()} | {error, term()}'
-spec start_link(Name :: atom(), Init :: errm_changeset_init()) -> {ok, pid()} | {error, term()}.
start_link(Name, Init = #errm_changeset_init{}) ->
  gen_server:start_link({local, Name}, ?MODULE, Init, []).

% @doc Stops changeset server.
% @param Pid    Pid or registered name of changeset server.
% @returns `ok'
-spec stop(Pid :: errm_server_ref()) -> ok.
stop(Pid) ->
  gen_server:stop(Pid).

% @doc Adds all changesets to the database.
% @param Pid    Pid or name of changeset server.
% @returns `ok | {error, errm_error()}'
% @equiv up(Pid, all)
-spec up(Pid :: errm_server_ref()) -> ok | {error, errm_error()}.
up(Pid) ->
  up(Pid, all).

% @doc Adds all changesets up to and including the changeset with ID `Id'.
% @param Pid    Pid or name of changeset server.
% @param Id     ID of changeset to add changes up to (inclusive).
% @returns `ok | {error, errm_error()}'
% @equiv up(Pid, Id, 120000)
-spec up(Pid :: errm_server_ref(), Id :: string() | all) -> ok | {error, errm_error()}.
up(Pid, Id) ->
  up(Pid, Id, ?ERRM_CHANGESET_DEFAULT_TIMEOUT).

% @doc Adds all changesets up to and including the changeset with ID `Id', timing out after `Timeout' milliseconds.
% @param Pid      Pid or name of changeset server.
% @param Id       ID of changeset to add changes up to (inclusive).
% @param Timeout  Number of milliseconds to wait for changesets to be added before timing out.
% @returns `ok | {error, errm_error()}'
-spec up(Pid :: errm_server_ref(), Id :: string() | all, Timeout :: pos_integer()) -> ok | {error, errm_error()}.
up(Pid, Id, Timeout) when is_list(Id) orelse Id =:= all, is_integer(Timeout), Timeout > 0 ->
  gen_server:call(Pid, {up, Id}, Timeout).

% @doc Removes all changesets from the database.
% @param Pid    Pid or name of changeset server.
% @returns `ok | {error, errm_error()}'
% @equiv down(Pid, all)
-spec down(Pid :: errm_server_ref()) -> ok | {error, errm_error()}.
down(Pid) ->
  down(Pid, all).

% @doc Removes all changesets down to and including the changeset with ID `Id'.
% @param Pid    Pid or name of changeset server.
% @param Id     ID of changeset to remove changes down to (inclusive).
% @returns `ok | {error, errm_error()}'
% @equiv down(Pid, Id, 120000)
-spec down(Pid :: errm_server_ref(), Id :: string() | all) -> ok | {error, errm_error()}.
down(Pid, Id) ->
  down(Pid, Id, ?ERRM_CHANGESET_DEFAULT_TIMEOUT).

% @doc Removes all changesets down to and including the changeset with ID `Id', timing out after `Timeout' milliseconds.
% @param Pid      Pid or name of changeset server.
% @param Id       ID of changeset to remove changes down to (inclusive).
% @param Timeout  Number of milliseconds to wait for changesets to be removed before timing out.
% @returns `ok | {error, errm_error()}'
-spec down(Pid :: errm_server_ref(), Id :: string() | all, Timeout :: pos_integer()) -> ok | {error, errm_error()}.
down(Pid, Id, Timeout) when is_list(Id) orelse Id =:= all, is_integer(Timeout), Timeout > 0 ->
  gen_server:call(Pid, {down, Id}, Timeout).

% @doc Get local status of changesets.
% @param Pid    Pid or name of changeset server.
% @returns `{ok, [errm_changeset()]} | {error, errm_error()}'
-spec status(Pid :: errm_server_ref()) -> {ok, [errm_changeset()]} | {error, errm_error()}.
status(Pid) ->
  gen_server:call(Pid, status).

%% gen_server callbacks

init(#errm_changeset_init{ path=Path, schema=Schema, table_name=Table, driver=Driver })
  when is_list(Path), is_list(Table), is_list(Schema) ->
  ChangeFiles = get_changes(Path),
  Tid = create_internal_table(),
  populate_table(Path, Tid, ChangeFiles),
  State = #errm_changeset_state{ changesets=Tid, schema=Schema, table_name=Table, driver=Driver },
  ok = create_table_if_nx(State),
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

handle_call({up, Id}, _From, State) ->
  Res = handle_up(Id, State),
  {reply, Res, State};
handle_call({down, Id}, _From, State) ->
  Res = handle_down(Id, State),
  {reply, Res, State};
handle_call(status, _From, State) ->
  Res = handle_status(State),
  {reply, Res, State};
handle_call(_Req, _From, State) ->
  {noreply, State}.

handle_cast(_Cast, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

%% private methods

-spec get_changes(Path :: string()) -> [string()].
get_changes(Path) ->
  {ok, Filenames} = file:list_dir(Path),
  filter_changes(Filenames).

-spec filter_changes(Filenames :: [string()]) -> [string()].
filter_changes(Filenames) ->
  lists:filter(fun filter_change/1, Filenames).

-spec filter_change(Filename :: string()) -> boolean().
filter_change(Filename) ->
  filename:extension(Filename) =:= ".yaml".

-spec create_internal_table() -> ets:table().
create_internal_table() ->
  ets:new(?ERRM_CHANGESET_TABLE, [ordered_set, protected, {keypos, 2}]).

-spec populate_table(Path :: string(), Tid :: ets:table(), ChangeFiles :: [string()]) -> ok.
populate_table(_Path, _Tid, []) ->
  ok;
populate_table(Path, Tid, [ChangeFile | Rest]) ->
  [PropLists] = yamerl_constr:file(filename:join([Path, ChangeFile])),
  Name = unicode_to_string(string:slice(ChangeFile, 0, string:length(ChangeFile) - 5)),
  Parsed = changeset_from_map(parse_change_specs(maps:from_list(PropLists), #{ "name" => Name, "status" => unknown })),
  true = ets:insert(Tid, Parsed),
  populate_table(Path, Tid, Rest).

-spec unicode_to_string(Name :: unicode:chardata()) -> string().
unicode_to_string(Name) when is_list(Name) ->
  Name.

parse_change_specs(#{ "up" := UpSpec, "down" := DownSpec }, Change) ->
  UpChange = parse_change_spec(up, maps:from_list(UpSpec)),
  DownChange = parse_change_spec(down, maps:from_list(DownSpec)),
  maps:merge(Change, maps:merge(UpChange, DownChange)).

parse_change_spec(up, #{ "sql" := Sql, "transaction" := Tx, "timeout" := Timeout }) ->
  OptsRec = changeset_options_from_map(#{ "transaction" => Tx, "timeout" => Timeout }),
  #{ "up_sql" => Sql, "up_options" => OptsRec };
parse_change_spec(down, #{ "sql" := Sql, "transaction" := Tx, "timeout" := Timeout }) ->
  OptsRec = changeset_options_from_map(#{ "transaction" => Tx, "timeout" => Timeout }),
  #{ "down_sql" => Sql, "down_options" => OptsRec }.

-spec changeset_from_map(Map :: maps:map()) -> errm_changeset().
changeset_from_map(#{
  "name" := Name,
  "up_sql" := UpSql,
  "down_sql" := DownSql,
  "up_options" := UpOpts=#errm_changeset_options{},
  "down_options" := DownOpts=#errm_changeset_options{},
  "status" := Status
}) when is_list(Name), is_list(UpSql), is_list(DownSql) ->
  #errm_changeset{ name=Name, up_sql=UpSql, up_options=UpOpts, down_sql=DownSql, down_options=DownOpts, status=Status }.

-spec changeset_options_from_map(Map :: maps:map()) -> errm_changeset_options().
changeset_options_from_map(#{
  "transaction" := Bool,
  "timeout" := Timeout
}) when is_boolean(Bool), is_integer(Timeout), Timeout > 0 ->
  #errm_changeset_options{ transaction=Bool, timeout=Timeout }.

-spec create_table_if_nx(State :: errm_changeset_state()) -> ok.
create_table_if_nx(State=#errm_changeset_state{ driver=Driver }) ->
  {ok, Conn} = errm_driver:get_connection(Driver),
  try
    ok = create_changeset_table(Conn, State),
    ok
  after
    errm_driver:release_connection(Driver, Conn)
  end.

-spec handle_up(Id :: string() | all, State :: errm_changeset_state()) -> ok | {error, errm_error()}.
handle_up(all, State=#errm_changeset_state{ changesets=Changes }) ->
  ToRun = ets:tab2list(Changes),
  try_up_changesets(ToRun, State);
handle_up(Id, State=#errm_changeset_state{ changesets=Changes }) ->
  case changeset_exists(Id, Changes) of
    true ->
      ToRun = changesets_less_than(Id, Changes),
      try_up_changesets(ToRun, State);
    false ->
      Err = string:join(["changeset", Id, "does not exist"], " "),
      {error, errm_error:new(?ERRM_CHANGESET_ERROR, Err)}
  end.

-spec changesets_less_than(Id :: string(), Tid :: ets:table()) -> [errm_changeset()].
changesets_less_than(Id, Tid) ->
  % dialyzer does not appreciate records and ets match statements, hack our way through this single function and keep
  % it isolated
  Match = {errm_changeset, '$1', '_', '_', '_', '_', '_'},
  ets:select(Tid, [{Match, [{'=<', '$1', Id}], ['$_']}]).

-spec create_changeset_table(Conn :: errm_connection(), State :: errm_changeset_state()) -> ok.
create_changeset_table(Conn, State) ->
  TableName = prepare_table_name(State),
  Query = ?STMT_F([?CREATE_TABLE_IF_NOT_EXISTS(TableName), "(name TEXT PRIMARY KEY)"]),
  {ok, []} = ?ERRM_TRY(errm_connection:query(Conn, Query, errm_query_options:new([]))),
  ok.

-spec try_up_changesets(ToRun :: [errm_changeset()], State :: errm_changeset_state()) -> ok | {error, errm_error()}.
try_up_changesets(ToRun, State=#errm_changeset_state{ driver=Driver }) ->
  {ok, Conn} = errm_driver:get_connection(Driver),
  try
    up_changesets(Conn, ToRun, State)
  catch
    error:{error, Err=#errm_error{}} ->
      {error, Err};
    Class:Exception ->
      erlang:Class(Exception)
  after
    errm_driver:release_connection(Driver, Conn)
  end.

-spec up_changesets(
  Conn :: errm_connection(),
  Changes :: [errm_changeset()],
  State :: errm_changeset_state()
) -> ok | {error, errm_error()}.
up_changesets(_Conn, [], _State) ->
  ok;
up_changesets(Conn, [Migration | Rest], State) ->
  case up_changeset(Conn, Migration, State) of
    ok -> up_changesets(Conn, Rest, State);
    {error, Err} -> {error, Err}
  end.

-spec up_changeset(
  Conn :: errm_connection(),
  Change :: errm_changeset(),
  State :: errm_changeset_state()
) -> ok | {error, errm_error()}.
up_changeset(Conn, Change=#errm_changeset{ up_options=UpOpts }, State=#errm_changeset_state{ changesets=M }) ->
  QueryOpts = changeset_start(Conn, UpOpts),
  try
    {ok, Ran} = ?ERRM_TRY(check_changeset_ran(Conn, QueryOpts, Change, State)),
    case Ran of
      true ->
        ok = ?ERRM_TRY(changeset_commit(Conn, QueryOpts)),
        true = ets:insert(M, Change#errm_changeset{ status=up }),
        ok;
      false ->
        ok = ?ERRM_TRY(do_changeset(Conn, QueryOpts, Change, State)),
        ok = ?ERRM_TRY(changeset_commit(Conn, QueryOpts)),
        true = ets:insert(M, Change#errm_changeset{ status=up }),
        ok
    end
  catch
    error:{error, Err=#errm_error{}} ->
      changeset_revert(Conn, QueryOpts),
      {error, Err};
    Class:Exception ->
      changeset_revert(Conn, QueryOpts),
      erlang:Class(Exception)
  end.

-spec do_changeset(
  Conn :: errm_connection(),
  QueryOpts :: errm_query_options(),
  Change :: errm_changeset(),
  State :: errm_changeset_state()
) -> ok | {error, errm_error()}.
do_changeset(Conn, QueryOpts, #errm_changeset{ name=Name, up_sql=Sql, up_options=UpOpts }, State) ->
  #errm_changeset_options{ timeout=Timeout } = UpOpts,
  ExecOpts = #errm_exec_options{ transaction=QueryOpts#errm_query_options.transaction },
  case errm_connection:exec(Conn, Sql, ExecOpts, Timeout) of
    ok -> insert_changeset_into_table(Conn, QueryOpts, Name, State);
    {error, Err} -> {error, Err}
  end.

-spec insert_changeset_into_table(
    Conn :: errm_connection(),
    QueryOpts :: errm_query_options(),
    Name :: string(),
    State :: errm_changeset_state()
) -> ok | {error, errm_error()}.
insert_changeset_into_table(Conn, QueryOpts, Name, State) ->
  TableName = prepare_table_name(State),
  Query = ?STMT_F([?INSERT_INTO(TableName), ?VALUES([["?"]])]),
  NQueryOpts = errm_query_options:parameters([Name], QueryOpts),
  case errm_connection:query(Conn, Query, NQueryOpts) of
    {ok, _} -> ok;
    {error, Err} -> {error, Err}
  end.

-spec handle_down(Id :: string() | all, State ::errm_changeset_state()) -> ok | {error, errm_error()}.
handle_down(all, State=#errm_changeset_state{ changesets=Changes }) ->
  ToRun = ets:tab2list(Changes),
  try_down_changesets(lists:reverse(ToRun), State);
handle_down(Id, State=#errm_changeset_state{ changesets=Changes }) ->
  case changeset_exists(Id, Changes) of
    true ->
      ToRun = changesets_greater_than(Id, Changes),
      try_down_changesets(ToRun, State);
    false ->
      Err = string:join(["changeset", Id, "does not exist"], " "),
      {error, errm_error:new(?ERRM_CHANGESET_ERROR, Err)}
  end.

-spec changeset_exists(Id :: string(), Tid :: ets:table()) -> boolean().
changeset_exists(Id, Tid) ->
  case ets:lookup(Tid, Id) of
    [#errm_changeset{ name=Id }] -> true;
    _ -> false
  end.

-spec changesets_greater_than(Id :: string(), Tid :: ets:table()) -> [errm_changeset()].
changesets_greater_than(Id, Tid) ->
  % dialyzer does not appreciate records and ets match statements, hack our way through this single function and keep
  % it isolated
  Match = {errm_changeset, '$1', '_', '_', '_', '_', '_'},
  ToRun = ets:select(Tid, [{Match, [{'>=', '$1', Id}], ['$_']}]),
  lists:reverse(ToRun).

-spec try_down_changesets(ToRun :: [errm_changeset()], State :: errm_changeset_state()) -> ok | {error, errm_error()}.
try_down_changesets(ToRun, State=#errm_changeset_state{ driver=Driver }) ->
  {ok, Conn} = errm_driver:get_connection(Driver),
  try
    down_changesets(Conn, ToRun, State)
  catch
    error:{error, Err=#errm_error{}} ->
      {error, Err};
    Class:Exception ->
      erlang:Class(Exception)
  after
    errm_driver:release_connection(Driver, Conn)
  end.

-spec down_changesets(
  Conn :: errm_connection(),
  Changes :: [errm_changeset()],
  State :: errm_changeset_state()
) -> ok | {error, errm_error()}.
down_changesets(_Conn, [], _State) ->
  ok;
down_changesets(Conn, [Change | Rest], State) ->
  case down_changeset(Conn, Change, State) of
    ok -> down_changesets(Conn, Rest, State);
    {error, Err} -> {error, Err}
  end.

-spec down_changeset(
  Conn :: errm_connection(),
  Change :: errm_changeset(),
  State :: errm_changeset_state()
) -> ok | {error, errm_error()}.
down_changeset(Conn, Change=#errm_changeset{ down_options=DownOpts }, State=#errm_changeset_state{ changesets=C }) ->
  QueryOpts = changeset_start(Conn, DownOpts),
  try
    {ok, Ran} = ?ERRM_TRY(check_changeset_ran(Conn, QueryOpts, Change, State)),
    case Ran of
      false ->
        ok = ?ERRM_TRY(changeset_commit(Conn, QueryOpts)),
        true = ets:insert(C, Change#errm_changeset{ status=down }),
        ok;
      true ->
        ok = ?ERRM_TRY(undo_changeset(Conn, QueryOpts, Change, State)),
        ok = ?ERRM_TRY(changeset_commit(Conn, QueryOpts)),
        true = ets:insert(C, Change#errm_changeset{ status=down }),
        ok
    end
  catch
    error:{error, Err=#errm_error{}} ->
      % attempt to rollback transaction, but response doesn't matter
      changeset_revert(Conn, QueryOpts),
      {error, Err};
    Class:Exception ->
      % attempt to rollback transaction, but response doesn't matter
      changeset_revert(Conn, QueryOpts),
      erlang:Class(Exception)
  end.

-spec undo_changeset(
  Conn :: errm_connection(),
  QueryOpts :: errm_query_options(),
  Change :: errm_changeset(),
  State :: errm_changeset_state()
) -> ok | {error, errm_error()}.
undo_changeset(Conn, QueryOpts, #errm_changeset{ name=Name, down_sql=Sql, down_options=DownOpts }, State) ->
  #errm_changeset_options{ timeout=Timeout } = DownOpts,
  ExecOpts = #errm_exec_options{ transaction=QueryOpts#errm_query_options.transaction },
  case errm_connection:exec(Conn, Sql, ExecOpts, Timeout) of
    ok ->
      delete_changeset_from_table(Conn, QueryOpts, Name, State);
    {error, Err} ->
      {error, Err}
  end.

-spec delete_changeset_from_table(
  Conn :: errm_connection(),
  QueryOpts :: errm_query_options(),
  Name :: string(),
  State :: errm_changeset_state()
) -> ok | {error, errm_error()}.
delete_changeset_from_table(Conn, QueryOpts, Name, State) ->
  TableName = prepare_table_name(State),
  Query = ?STMT_F([?DELETE_FROM(TableName), ?WHERE(?EQ("name", "?"))]),
  NQueryOpts = errm_query_options:parameters([Name], QueryOpts),
  case errm_connection:query(Conn, Query, NQueryOpts) of
    {ok, _} -> ok;
    {error, Err} -> {error, Err}
  end.

-spec check_changeset_ran(
  Conn :: errm_connection(),
  QueryOpts :: errm_query_options(),
  Change :: errm_changeset(),
  State :: errm_changeset_state()
) -> {ok, boolean()} | {error, errm_error()}.
check_changeset_ran(Conn, QueryOpts, #errm_changeset{ name=Name }, State) ->
  TableName = prepare_table_name(State),
  Query = ?STMT_F([?SELECT_FROM(["*"], TableName), ?WHERE(?EQ("name", "?"))]),
  NameBin = erlang:list_to_binary(Name),
  NQueryOpts = errm_query_options:parameters([Name], QueryOpts),
  case errm_connection:query(Conn, Query, NQueryOpts) of
    {ok, [[NameBin]]} -> {ok, true};
    {ok, []} -> {ok, false};
    {error, Err} -> {error, Err}
  end.

-spec changeset_start(Conn :: errm_connection(), Options :: errm_changeset_options()) -> errm_query_options().
changeset_start(_Conn, #errm_changeset_options{ transaction=false }) ->
  errm_query_options:new([]);
changeset_start(Conn, #errm_changeset_options{ transaction=true }) ->
  {ok, TxRef} = ?ERRM_TRY(errm_connection:start_transaction(Conn, "READ COMMITTED")),
  errm_query_options:new([], TxRef).

-spec changeset_revert(Conn :: errm_connection(), Options :: errm_query_options()) -> ok | {error, errm_error()}.
changeset_revert(_Conn, #errm_query_options{ transaction=undefined }) ->
  ok;
changeset_revert(Conn, #errm_query_options{ transaction=TxRef }) ->
  errm_connection:rollback_transaction(Conn, TxRef).

-spec changeset_commit(Conn :: errm_connection(), Options :: errm_query_options()) -> ok | {error, errm_error()}.
changeset_commit(_Conn, #errm_query_options{ transaction=undefined }) ->
  ok;
changeset_commit(Conn, #errm_query_options{ transaction=TxRef }) ->
  errm_connection:commit_transaction(Conn, TxRef).

-spec prepare_table_name(State :: errm_changeset_state()) -> string().
prepare_table_name(#errm_changeset_state{ schema=Schema, table_name=Table }) ->
  string:join([Schema, Table], ".").

-spec handle_status(State :: errm_changeset_state()) -> {ok, [errm_changeset()]}.
handle_status(#errm_changeset_state{ changesets=C }) ->
  {ok, ets:tab2list(C)}.