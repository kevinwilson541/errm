-module(errm_sqlite_driver).

-include("errm_error.hrl").
-include("errm_gen_driver.hrl").

-behavior(errm_gen_driver).

%% API
-export([
  start_link/1,
  stop/1,
  query/4,
  exec/3,
  start_transaction/3,
  commit_transaction/2,
  rollback_transaction/2,
  savepoint/3,
  release_savepoint/3,
  revert_savepoint/3
]).

-spec start_link(Init :: list(term())) -> {ok, term()}.
start_link([File]) ->
  esqlite3:open(File).

-spec stop(Pid :: term()) -> ok.
stop(Pid) ->
  esqlite3:close(Pid).

-spec query(Pid :: term(), Query :: iodata(), Params :: list(), Timeout :: pos_integer()) -> {ok, errm_query_reply()} | {error, errm_error()}.
query(Pid, Query, Params, _Timeout) ->
  case esqlite3:q(Pid, Query, Params) of
    Rows when is_list(Rows) -> {ok, Rows};
    {error, Code} when is_integer(Code) ->
      Message = string:join(["query failed in driver with code", integer_to_list(Code)], " "),
      ErrorInfo = esqlite3:error_info(Pid),
      Err = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, Err};
    {error, _DriverError} ->
      Message = "query failed in driver",
      ErrorInfo = esqlite3:error_info(Pid),
      UnkErr = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, UnkErr}
  end.

-spec exec(Pid :: term(), Query :: iodata(), Timeout :: pos_integer()) -> ok | {error, errm_error()}.
exec(Pid, Query, _Timeout) ->
  case esqlite3:exec(Pid, Query) of
    ok -> ok;
    {error, Code} when is_integer(Code) ->
      Message = string:join(["execution failed in driver with code", integer_to_list(Code)], " "),
      ErrorInfo = esqlite3:error_info(Pid),
      Err = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, Err};
    {error, _DriverError} ->
      Message = "execution failed in driver",
      ErrorInfo = esqlite3:error_info(Pid),
      UnkErr = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, UnkErr}
  end.

-spec start_transaction(Pid :: term(), Isolation :: iodata(), Timeout :: pos_integer()) -> ok | {error, errm_error()}.
start_transaction(Pid, _IsolationLevel, _Timeout) ->
  case esqlite3:exec(Pid, "BEGIN") of
    ok -> ok;
    {error, Code} when is_integer(Code) ->
      Message = string:join(["transaction start failed in driver with code", integer_to_list(Code)], " "),
      ErrorInfo = esqlite3:error_info(Pid),
      Err = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, Err};
    {error, _DriverError} ->
      Message = "transaction start failed in driver",
      ErrorInfo = esqlite3:error_info(Pid),
      UnkErr = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, UnkErr}
  end.

-spec commit_transaction(Pid :: term(), Timeout :: pos_integer()) -> ok | {error, errm_error()}.
commit_transaction(Pid, _Timeout) ->
  case esqlite3:exec(Pid, "COMMIT") of
    ok -> ok;
    {error, Code} when is_integer(Code) ->
      Message = string:join(["transaction commit failed in driver with code", integer_to_list(Code)], " "),
      ErrorInfo = esqlite3:error_info(Pid),
      Err = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, Err};
    {error, _DriverError} ->
      Message = "transaction commit failed in driver",
      ErrorInfo = esqlite3:error_info(Pid),
      UnkErr = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, UnkErr}
  end.

-spec rollback_transaction(Pid :: term(), Timeout :: pos_integer()) -> ok | {error, errm_error()}.
rollback_transaction(Pid, _Timeout) ->
  case esqlite3:exec(Pid, "ROLLBACK") of
    ok -> ok;
    {error, Code} when is_integer(Code) ->
      Message = string:join(["transaction rollback failed in driver with code", integer_to_list(Code)], " "),
      ErrorInfo = esqlite3:error_info(Pid),
      Err = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, Err};
    {error, _DriverError} ->
      Message = "transaction rollback failed in driver",
      ErrorInfo = esqlite3:error_info(Pid),
      UnkErr = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, UnkErr}
  end.

-spec savepoint(Pid :: term(), Savepoint :: iodata(), Timeout :: pos_integer()) -> ok | {error, errm_error()}.
savepoint(Pid, Savepoint, _Timeout) ->
  case esqlite3:exec(Pid, ["SAVEPOINT ", Savepoint]) of
    ok -> ok;
    {error, Code} when is_integer(Code) ->
      Message = string:join(["transaction savepoint failed in driver with code", integer_to_list(Code)], " "),
      ErrorInfo = esqlite3:error_info(Pid),
      Err = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, Err};
    {error, _DriverError} ->
      Message = "transaction savepoint failed in driver",
      ErrorInfo = esqlite3:error_info(Pid),
      UnkErr = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, UnkErr}
  end.

-spec release_savepoint(Pid :: term(), Savepoint :: iodata(), Timeout :: pos_integer()) -> ok | {error, errm_error()}.
release_savepoint(Pid, Savepoint, _Timeout) ->
  case esqlite3:exec(Pid, ["RELEASE SAVEPOINT ", Savepoint]) of
    ok -> ok;
    {error, Code} when is_integer(Code) ->
      Message = string:join(["transaction savepoint release failed in driver with code", integer_to_list(Code)], " "),
      ErrorInfo = esqlite3:error_info(Pid),
      Err = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, Err};
    {error, _DriverError} ->
      Message = "transaction savepoint release failed in driver",
      ErrorInfo = esqlite3:error_info(Pid),
      UnkErr = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, UnkErr}
  end.

-spec revert_savepoint(Pid :: term(), Savepoint :: iodata(), Timeout :: pos_integer()) -> ok | {error, errm_error()}.
revert_savepoint(Pid, Savepoint, _Timeout) ->
  case esqlite3:exec(Pid, ["ROLLBACK TO SAVEPOINT ", Savepoint]) of
    ok -> ok;
    {error, Code} when is_integer(Code) ->
      Message = string:join(["transaction savepoint rollback failed in driver with code", integer_to_list(Code)], " "),
      ErrorInfo = esqlite3:error_info(Pid),
      Err = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, Err};
    {error, _DriverError} ->
      Message = "transaction savepoint rollback failed in driver",
      ErrorInfo = esqlite3:error_info(Pid),
      UnkErr = errm_error:new(?ERRM_QUERY_FAILED_ERROR, Message, {error, ErrorInfo}),
      {error, UnkErr}
  end.