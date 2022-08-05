-module(errm_result).

-include("errm_error.hrl").

%% API
-export([
  ok/1,
  ok_empty/0,
  error/1,
  assert/1,
  assert_ok/1,
  assert_error/1
]).

% @doc Create OK value for errm result.
% @param Val    Value to set on errm result.
% @returns `{ok, term()}'
-spec ok(Val :: term()) -> {ok, term()}.
ok(Val) ->
  {ok, Val}.

% @doc Create empty OK value for errm result.
% @returns `ok'
-spec ok_empty() -> ok.
ok_empty() ->
  ok.

% @doc Create Error value for errm result.
% @param Err    Error term to set on errm result.
% @returns `{error, term()}'
-spec error(Err :: term()) -> {error, term()}.
error(Err) ->
  {error, Err}.

% @doc Assert that the errm result is an OK value. Will throw if the input result value is an error tuple.
% @param Result   errm result value to assert OK on.
% returns `ok | {ok, term()}'
-spec assert(Result :: ok | {ok, term()} | {error, term()}) -> ok | {ok, term()}.
assert(ok) ->
  ok;
assert({ok, Val}) ->
  {ok, Val};
assert({error, Err}) ->
  erlang:error({error, Err}).

% @doc Assert that the errm result is an OK value, and return empty OK. Will throw if the input result value is an error tuple.
% @param Result   errm result value to assert OK on.
% returns `ok'
-spec assert_ok(Result :: ok | {ok, term()} | {error, term()}) -> ok.
assert_ok(ok) ->
  ok;
assert_ok({ok, _Val}) ->
  ok;
assert_ok({error, Err}) ->
  erlang:error({error, Err}).

% @doc Assert that the errm result is an Error value. Will throw if the input result value is an OK tuple.
% @param Result   errm result value to assert Error on.
% returns `{error, term()}'
-spec assert_error(Result :: ok | {ok, term()} | {error, term()}) -> {error, term()}.
assert_error(ok) ->
  erlang:error({error, errm_error:new(?ERRM_ASSERT_ERROR, "value is not an error")});
assert_error({ok, _Val}) ->
  erlang:error({error, errm_error:new(?ERRM_ASSERT_ERROR, "value is not an error")});
assert_error({error, Err}) ->
  {error, Err}.