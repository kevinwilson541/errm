-module(errm_connection).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("errm_connection.hrl").
-include("errm_error.hrl").
-include("errm_gen_driver.hrl").
-include("errm_result.hrl").

-behavior(gen_server).

%% public api
-export([
  start_link/1,
  stop/1,
  acquire/1,
  acquire/2,
  release/1,
  release/2,
  query/2,
  query/3,
  query/4,
  exec/2,
  exec/3,
  exec/4,
  start_transaction/2,
  start_transaction/3,
  commit_transaction/2,
  commit_transaction/3,
  rollback_transaction/2,
  rollback_transaction/3,
  transaction/3,
  transaction/4
]).
%% gen_server behavior
-export([
  init/1,
  terminate/2,
  handle_call/3,
  handle_cast/2,
  handle_info/2
]).

%% public api

% @doc Starts and links connection server with initialization parameter containing driver module and driver init parameters.
% @param Init   Initialization record containing driver module and driver init parameters, to initialize managed `errm_gen_driver' implementation.
% @returns `{ok, errm_connection()} | {error, term()}'
% @see errm_gen_driver:start_link/1
-spec start_link(Init :: errm_connection_init()) -> {ok, errm_connection()} | {error, term()}.
start_link(Init=#errm_connection_init{}) ->
  gen_server:start_link(?MODULE, Init, []).

% @doc Stops connection server.
% @param Conn   Pid of connection server.
% @returns `ok'
% @see errm_gen_driver:stop/1
-spec stop(Conn:: errm_connection()) -> ok.
stop(Conn) ->
  gen_server:stop(Conn).

% @doc Pre-acquisition check to ensure no transaction contexts are found on connection server.
% @param Conn   Pid of connection server.
% @returns `ok | {error, errm_error()}'
% @equiv acquire(Conn, 5000)
-spec acquire(Conn :: errm_connection()) -> ok | {error, errm_error()}.
acquire(Conn) ->
  acquire(Conn, ?ERRM_CONN_DEFAULT_TIMEOUT).

% @doc Pre-acquisition check to ensure no transaction contexts are found on connection server, timing out after `Timeout'
% milliseconds.
% @param Conn       Pid of connection server.
% @param Timeout    Number of milliseconds to wait for response before timing out.
% returns `ok | {error, errm_error()}'
-spec acquire(Conn :: errm_connection(), Timeout :: pos_integer()) -> ok | {error, errm_error()}.
acquire(Conn, Timeout) ->
  gen_server:call(Conn, acquire, Timeout).

% @doc Pre-release check to ensure no transaction contexts are found on connection server.
% @param Conn   Pid of connection server.
% @returns `ok | {error, errm_error()}'
% @equiv release(Conn, 5000)
-spec release(Conn :: errm_connection()) -> ok | {error, errm_error()}.
release(Conn) ->
  release(Conn, ?ERRM_CONN_DEFAULT_TIMEOUT).

% @doc Pre-release check to ensure no transaction contexts are found on connection server, timing out after `Timeout'
% milliseconds.
% @param Conn       Pid of connection server.
% @param Timeout    Number of milliseconds to wait for response before timing out.
% @returns `ok | {error, errm_error()}'
-spec release(Conn :: errm_connection(), Timeout :: pos_integer()) -> ok | {error, errm_error()}.
release(Conn, Timeout) ->
  gen_server:call(Conn, release, Timeout).

% @doc Run query against connection server, calling the errm_gen_driver `query/4' method.
% @param Conn   Pid of connection server.
% @param Query  Query to run.
% @returns `{ok, errm_query_reply()} | {error, errm_error()}'
% @equiv query(Conn, Query, errm_query_options:new())
% @see errm_gen_driver:query/4
-spec query(Conn :: errm_connection(), Query :: iodata()) -> {ok, errm_query_reply()} | {error, errm_error()}.
query(Conn, Query) ->
  query(Conn, Query, errm_query_options:new()).

% @doc Run query against connection server, calling the errm_gen_driver `query/4' method with parameters and transaction
% reference specified in `Options'.
% @param Conn       Pid of connection server.
% @param Query      Query to run.
% @param Options    Record containing query parameters and transaction reference (if any).
% @returns `{ok, errm_query_reply()} | {error, errm_error()}'
% @equiv query(Conn, Query, Options, 5000)
% @see errm_gen_driver:query/4
-spec query(
  Conn :: errm_connection(),
  Query :: iodata(),
  Options :: errm_query_options()
) -> {ok, errm_query_reply()} | {error, errm_error()}.
query(Conn, Query, Opts) ->
  query(Conn, Query, Opts, ?ERRM_CONN_DEFAULT_TIMEOUT).

% @doc Run query against connection server, calling the errm_gen_driver `query/4' method with parameters and transaction
% reference specified in `Options', and timing out after `Timeout' milliseconds.
% @param Conn       Pid of connection server.
% @param Query      Query to run.
% @param Options    Record containing query parameters and transaction reference (if any).
% @param Timeout    Number of milliseconds to wait for query to run before timing out.
% @returns `{ok, errm_query_reply()} | {error, errm_error()}'
% @see errm_gen_driver:query/4
-spec query(
  Conn :: errm_connection(),
  Query :: iodata(),
  Options :: errm_query_options(),
  Timeout :: pos_integer()
) -> {ok, errm_query_reply()} | {error, errm_error()}.
query(Conn, Query, Opts, Timeout) ->
  gen_server:call(Conn, {query, Query, Opts, Timeout}, Timeout).

% @doc Execute a statement against connection server, calling the errm_gen_driver `exec/3' method.
% @param Conn   Pid of connection server.
% @param Query  Statement to run.
% @returns `ok | {error, errm_error()}'
% @equiv exec(Conn, Query, errm_exec_options:new())
% @see errm_gen_driver:exec/3
-spec exec(Conn :: errm_connection(), Query :: iodata()) -> ok | {error, errm_error()}.
exec(Conn, Query) ->
  exec(Conn, Query, errm_exec_options:new()).

% @doc Execute a statement against connection server, calling the errm_gen_driver `exec/3' method with transaction reference
% specified in `Options'.
% @param Conn       Pid of connection server.
% @param Query      Statement to run.
% @param Options    Record containing transaction reference for statement execution (if any).
% @returns `ok | {error, errm_error()}'
% @equiv exec(Conn, Query, errm_exec_options:new(), 5000)
% @see errm_gen_driver:exec/3
-spec exec(
  Conn :: errm_connection(),
  Query :: iodata(),
  Options :: errm_exec_options()
) -> ok | {error, errm_error()}.
exec(Conn, Query, Opts) ->
  exec(Conn, Query, Opts, ?ERRM_CONN_DEFAULT_TIMEOUT).

% @doc Execute a statement against connection server, calling the errm_gen_driver `exec/3' method with transaction reference
% specified in `Options', and timing out after `Timeout' milliseconds.
% @param Conn       Pid of connection server.
% @param Query      Statement to run.
% @param Options    Record containing transaction reference for statement execution (if any).
% @param Timeout    Number of milliseconds to wait for statement execution to run before timing out.
% @returns `ok | {error, errm_error()}'
% @see errm_gen_driver:exec/3
-spec exec(
  Conn :: errm_connection(),
  Query :: iodata(),
  Options :: errm_exec_options(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.
exec(Conn, Query, Opts, Timeout) ->
  gen_server:call(Conn, {exec, Query, Opts, Timeout}, Timeout).

% @doc Start a transaction against connection server.
%
% If this is the first transaction started after acquiring connection `Conn', the errm_gen_driver `start_transaction/3'
% method will be called. Otherwise, the errm_gen_driver `savepoint/3' method will be called to start a sub-transaction.
%
% @param Conn             Pid of connection server.
% @param IsolationLevel   Isolation level to apply to transaction (if applicable).
% @returns `{ok, errm_transaction()} | {error, errm_error()}'
% @equiv start_transaction(Conn, IsolationLevel, 5000)
% @see errm_gen_driver:start_transaction/3
% @see errm_gen_driver:savepoint/3
-spec start_transaction(
  Conn :: errm_connection(),
  IsolationLevel :: iodata()
) -> {ok, errm_transaction()} | {error, errm_error()}.
start_transaction(Conn, IsolationLevel) ->
  start_transaction(Conn, IsolationLevel, ?ERRM_CONN_DEFAULT_TIMEOUT).

% @doc Start a transaction against connection server, timing out after `Timeout' milliseconds.
%
% If this is the first transaction started after acquiring connection `Conn', the errm_gen_driver `start_transaction/3'
% method will be called. Otherwise, the errm_gen_driver `savepoint/3' method will be called to start a sub-transaction.
%
% @param Conn             Pid of connection server.
% @param IsolationLevel   Isolation level to apply to transaction (if applicable).
% @param Timeout          Number of milliseconds to wait for transaction to start before timing out.
% @returns `{ok, errm_transaction()} | {error, errm_error()}'
% @see errm_gen_driver:start_transaction/3
% @see errm_gen_driver:savepoint/3
-spec start_transaction(
  Conn :: errm_connection(),
  IsolationLevel :: iodata(),
  Timeout :: pos_integer()
) -> {ok, errm_transaction()} | {error, errm_error()}.
start_transaction(Conn, IsolationLevel, Timeout) ->
  gen_server:call(Conn, {start_transaction, IsolationLevel, Timeout}, Timeout).

% @doc Commit a transaction, identified by `TxRef', on the connection server.
%
% If this is the first transaction started after acquiring connection `Conn', the errm_gen_driver `commit_transaction/2'
% method will be called. Otherwise, the errm_gen_driver `release_savepoint/3' method will be called to commit a sub-transaction.
%
% @param Conn     Pid of connection server.
% @param TxRef    Transaction ID to commit.
% @returns `ok | {error, errm_error()}'
% @equiv commit_transaction(Conn, TxRef, 5000)
% @see errm_gen_driver:commit_transaction/2
% @see errm_gen_driver:release_savepoint/3
-spec commit_transaction(Conn :: errm_connection(), TxRef :: errm_transaction()) -> ok | {error, errm_error()}.
commit_transaction(Conn, TxRef) ->
  commit_transaction(Conn, TxRef, ?ERRM_CONN_DEFAULT_TIMEOUT).

% @doc Commit a transaction, identified by `TxRef', on the connection server, timing out after `Timeout' milliseconds.
%
% If this is the first transaction started after acquiring connection `Conn', the errm_gen_driver `commit_transaction/2'
% method will be called. Otherwise, the errm_gen_driver `release_savepoint/3' method will be called to commit a sub-transaction.
%
% @param Conn       Pid of connection server.
% @param TxRef      Transaction ID to commit.
% @param Timeout    Number of milliseconds to wait for transaction to commit before timing out.
% @returns `ok | {error, errm_error()}'
% @see errm_gen_driver:commit_transaction/2
% @see errm_gen_driver:release_savepoint/3
-spec commit_transaction(
  Conn :: errm_connection(),
  TxRef :: errm_transaction(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.
commit_transaction(Conn, TxRef, Timeout) ->
  gen_server:call(Conn, {commit_transaction, TxRef, Timeout}, Timeout).

% @doc Rollback a transaction, identified by `TxRef', on the connection server.
%
% If this is the first transaction started after acquiring connection `Conn', the errm_gen_driver `rollback_transaction/2'
% method will be called. Otherwise, the errm_gen_driver `revert_savepoint/3' method will be called to rollback a sub-transaction.
%
% @param Conn     Pid of connection server.
% @param TxRef    Transaction ID to rollback.
% @returns `ok | {error, errm_error()}'
% @equiv rollback_transaction(Conn, TxRef, 5000)
% @see errm_gen_driver:rollback_transaction/2
% @see errm_gen_driver:revert_savepoint/3
-spec rollback_transaction(Conn :: errm_connection(), TxRef :: errm_transaction()) -> ok | {error, errm_error()}.
rollback_transaction(Conn, TxRef) ->
  rollback_transaction(Conn, TxRef, ?ERRM_CONN_DEFAULT_TIMEOUT).

% @doc Rollback a transaction, identified by `TxRef', on the connection server, timing out after `Timeout' milliseconds.
%
% If this is the first transaction started after acquiring connection `Conn', the errm_gen_driver `rollback_transaction/2'
% method will be called. Otherwise, the errm_gen_driver `revert_savepoint/3' method will be called to rollback a sub-transaction.
%
% @param Conn       Pid of connection server.
% @param TxRef      Transaction ID to rollback.
% @param Timeout    Number of milliseconds to wait for transaction to rollback before timing out.
% @returns `ok | {error, errm_error()}'
% @see errm_gen_driver:rollback_transaction/2
% @see errm_gen_driver:revert_savepoint/3
-spec rollback_transaction(
  Conn :: errm_connection(),
  TxRef :: errm_transaction(),
  Timeout :: pos_integer()
) -> ok | {error, errm_error()}.
rollback_transaction(Conn, TxRef, Timeout) ->
  gen_server:call(Conn, {rollback_transaction, TxRef, Timeout}, Timeout).

% @doc Run a transaction on connection `Conn', passing this transaction to functional context `Fn'.
%
% If `Fn' returns `{ok, term()', this method will attempt to commit the transaction. If `Fn' returns
% `{error, errm_error()}', this method will attempt to rollback the transaction. If transaction commit fails, this method
% will attempt to rollback the transaction. In any case, if transaction rollback fails, then this method will raise an
% exception.
%
% @param Conn       Pid of connection server.
% @param IsolationLevel   Isolation level to run transaction on (if applicable).
% @param Fn               Function to execute with `Conn' and `TxRef' context, in order to issue queries against.
% @returns `{ok, term()} | {error, errm_error()}'
-spec transaction(
  Conn :: errm_connection(),
  IsolationLevel :: iodata(),
  Fn :: fun((Conn :: errm_connection(), TxRef :: errm_transaction()) -> {ok, term()} | {error, errm_error()})
) -> {ok, term()} | {error, errm_error()}.
transaction(Conn, IsolationLevel, Fn) ->
  case start_transaction(Conn, IsolationLevel) of
    {ok, TxRef} -> do_transaction(Conn, TxRef, Fn);
    {error, Err} -> {error, Err}
  end.

-spec transaction(
  Conn :: errm_connection(),
  IsolationLevel :: iodata(),
  Timeout :: pos_integer(),
  Fn :: fun((Conn :: errm_connection(), TxRef :: errm_transaction()) -> {ok, term()} | {error, errm_error()})
) -> {ok, term()} | {error, errm_error()}.
transaction(Conn, IsolationLevel, Timeout, Fn) ->
  case start_transaction(Conn, IsolationLevel, Timeout) of
    {ok, TxRef} -> do_transaction(Conn, TxRef, Fn);
    {error, Err} -> {error, Err}
  end.

%% gen_server callbacks

init(#errm_connection_init{ module=Mod, args=Args }) ->
  {ok, Pid} = Mod:start_link(Args),
  {ok, #errm_connection_state{ module=Mod, driver=Pid, transaction_stack=[] }}.

terminate(_Reason, #errm_connection_state{ module=Mod, driver=Pid }) ->
  Mod:stop(Pid),
  ok.

handle_call(acquire, _From, State) ->
  case ensure_no_transactions(State) of
    {error, Code} ->
      {stop, normal, {error, Code}, State};
    ok ->
      {reply, ok, State}
  end;
handle_call(release, _From, State) ->
  case ensure_no_transactions(State) of
    {error, Code} ->
      {stop, normal, {error, Code}, State};
    ok ->
      {reply, ok, State}
  end;
handle_call({query, Query, QueryOpts, Timeout}, _From, State=#errm_connection_state{ module=Mod, driver=Pid }) ->
  #errm_query_options{ parameters=Params, transaction=TxRef } = QueryOpts,
  case check_transaction(TxRef, State) of
    ok ->
      Res = Mod:query(Pid, Query, Params, Timeout),
      {reply, Res, State};
    {error, Err} ->
      {reply, {error, Err}, State}
  end;
handle_call({exec, Query, ExecOpts, Timeout}, _From, State=#errm_connection_state{ module=Mod, driver=Pid }) ->
  #errm_exec_options{ transaction=TxRef } = ExecOpts,
  case check_transaction(TxRef, State) of
    ok ->
      Res = Mod:exec(Pid, Query, Timeout),
      {reply, Res, State};
    {error, Err} ->
      {reply, {error, Err}, State}
  end;
handle_call({start_transaction, IsolationLevel, Timeout}, _From, State=#errm_connection_state{ transaction_stack=Txs }) ->
  case handle_start_transaction(IsolationLevel, Timeout, State) of
    ok ->
      TxRef = erlang:make_ref(),
      {reply, {ok, TxRef}, State#errm_connection_state{ transaction_stack=[TxRef | Txs] }};
    {error, Err} ->
      {reply, {error, Err}, State}
  end;
handle_call({commit_transaction, TxRef, Timeout}, _From, State=#errm_connection_state{ transaction_stack=Txs }) ->
  case handle_commit_transaction(TxRef, Timeout, State) of
    ok ->
      {reply, ok, State#errm_connection_state{ transaction_stack=tl(Txs) }};
    {error, Err} ->
      {reply, {error, Err}, State}
  end;
handle_call({rollback_transaction, TxRef, Timeout}, _From, State=#errm_connection_state{ transaction_stack=Txs }) ->
  case handle_rollback_transaction(TxRef, Timeout, State) of
    ok ->
      {reply, ok, State#errm_connection_state{ transaction_stack=tl(Txs) }};
    {error, Err} ->
      {reply, {error, Err}, State}
  end;
handle_call(_Req, _From, State) ->
  {noreply, State}.

handle_cast(_Req, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

%% private functions

-spec check_transaction(
  Tx :: errm_transaction() | undefined,
  State :: errm_connection_state()
) -> ok | {error, errm_error()}.
check_transaction(undefined, #errm_connection_state{ transaction_stack=[] }) ->
  ok;
check_transaction(TxRef, #errm_connection_state{ transaction_stack=[TxRef | _Rest] }) ->
  ok;
check_transaction(undefined, _State) ->
  Err = errm_error:new(?ERRM_TRANSACTION_MISMATCH_ERROR, "query not bound to current transaction"),
  {error, Err};
check_transaction(_TxRef, #errm_connection_state{ transaction_stack=[] }) ->
  Err = errm_error:new(?ERRM_TRANSACTION_NOT_FOUND_ERROR, "transaction for query does not exist"),
  {error, Err};
check_transaction(_TxRef, _State) ->
  Err = errm_error:new(?ERRM_TRANSACTION_MISMATCH_ERROR, "transaction for query does not match current transaction"),
  {error, Err}.

-spec ensure_no_transactions(State :: errm_connection_state()) -> ok | {error, errm_error()}.
ensure_no_transactions(#errm_connection_state{ transaction_stack=[] }) ->
  ok;
ensure_no_transactions(_) ->
  Err = errm_error:new(?ERRM_TRANSACTION_LEAK_ERROR, "existing transactions found on connection, closing"),
  {error, Err}.

-spec handle_start_transaction(
  IsolationLevel :: iodata(),
  Timeout :: pos_integer(),
  State :: errm_connection_state()
) -> ok | {error, errm_error()}.
handle_start_transaction(IsolationLevel, Timeout, #errm_connection_state{ module=Mod, driver=Pid, transaction_stack=[] }) ->
  Mod:start_transaction(Pid, IsolationLevel, Timeout);
handle_start_transaction(_IsolationLevel, Timeout, #errm_connection_state{ module=Mod, driver=Pid, transaction_stack=Txs }) ->
  Mod:savepoint(Pid, [?ERRM_CONN_SAVEPOINT, integer_to_list(length(Txs))], Timeout).

-spec handle_commit_transaction(
  TxRef :: errm_transaction(),
  Timeout :: pos_integer(),
  State :: errm_connection_state()
) -> ok | {error, errm_error()}.
handle_commit_transaction(TxRef, Timeout, #errm_connection_state{ module=Mod, driver=Pid, transaction_stack=[TxRef] }) ->
  Mod:commit_transaction(Pid, Timeout);
handle_commit_transaction(TxRef, Timeout, #errm_connection_state{ module=Mod, driver=Pid, transaction_stack=[TxRef | _]=Txs }) ->
  Mod:release_savepoint(Pid, [?ERRM_CONN_SAVEPOINT, integer_to_list(length(Txs) - 1)], Timeout);
handle_commit_transaction(_TxRef, _Timeout, #errm_connection_state{ transaction_stack=_ }) ->
  {error, errm_error:new(?ERRM_TRANSACTION_MISMATCH_ERROR, "transaction for commit does not match current transaction")}.

-spec handle_rollback_transaction(
  TxRef :: errm_transaction(),
  Timeout :: pos_integer(),
  State :: errm_connection_state()
) -> ok | {error, errm_error()}.
handle_rollback_transaction(TxRef, Timeout, #errm_connection_state{ module=Mod, driver=Pid, transaction_stack=[TxRef] }) ->
  Mod:rollback_transaction(Pid, Timeout);
handle_rollback_transaction(TxRef, Timeout, #errm_connection_state{ module=Mod, driver=Pid, transaction_stack=[TxRef | _]=Txs }) ->
  Mod:revert_savepoint(Pid, [?ERRM_CONN_SAVEPOINT, integer_to_list(length(Txs) - 1)], Timeout);
handle_rollback_transaction(_TxRef, _Timeout, #errm_connection_state{ transaction_stack=_ }) ->
  {error, errm_error:new(?ERRM_TRANSACTION_MISMATCH_ERROR, "transaction for rollback does not match current transaction")}.

-spec do_transaction(
  Conn :: errm_connection(),
  TxRef :: errm_transaction(),
  Fn :: fun((Conn :: errm_connection(), TxRef :: errm_transaction()) -> {ok, term()} | {error, errm_error()})
) -> {ok, term()} | {error, errm_error()}.
do_transaction(Conn, TxRef, Fn) ->
  try
    {ok, Res} = ?ERRM_TRY(Fn(Conn, TxRef)),
    ok = ?ERRM_TRY(commit_transaction(Conn, TxRef)),
    {ok, Res}
  catch
    error:{error, Err=#errm_error{}} ->
      ok = ?ERRM_TRY(rollback_transaction(Conn, TxRef)),
      {error, Err};
    Class:Exception ->
      ok = ?ERRM_TRY(rollback_transaction(Conn, TxRef)),
      erlang:Class(Exception)
  end.