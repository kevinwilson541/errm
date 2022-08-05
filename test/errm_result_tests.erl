-module(errm_result_tests).

-include("errm_result.hrl").
-include("errm_error.hrl").
-include_lib("eunit/include/eunit.hrl").

ok_test() ->
  Ret = errm_result:ok("value"),
  ?assertEqual({ok, "value"}, Ret).

ok_empty_test() ->
  Ret = errm_result:ok_empty(),
  ?assertEqual(ok, Ret).

error_test() ->
  Ret = errm_result:error("error message"),
  ?assertEqual({error, "error message"}, Ret).

assert_ok_fn_empty_test() ->
  Ret = errm_result:assert_ok(ok),
  ?assertEqual(ok, Ret).

assert_ok_fn_value_test() ->
  Ret = errm_result:assert_ok({ok, "value"}),
  ?assertEqual(ok, Ret).

assert_ok_fn_error_test() ->
  Err = {error, "error message"},
  ?assertError(Err, errm_result:assert_ok(Err)).

assert_fn_empty_test() ->
  Ret = errm_result:assert(ok),
  ?assertEqual(ok, Ret).

assert_fn_value_test() ->
  Ret = errm_result:assert({ok, "value"}),
  ?assertEqual({ok, "value"}, Ret).

assert_fn_error_test() ->
  Err = {error, "error message"},
  ?assertError(Err, errm_result:assert(Err)).

assert_error_fn_ok_test() ->
  Err = {error, #errm_error{ code=?ERRM_ASSERT_ERROR, message="value is not an error", description="assertion error" }},
  ?assertError(Err, errm_result:assert_error(ok)).

assert_error_fn_value_test() ->
  Err = {error, #errm_error{ code=?ERRM_ASSERT_ERROR, message="value is not an error", description="assertion error" }},
  ?assertError(Err, errm_result:assert_error({ok, "value"})).

assert_error_fn_error_test() ->
  Err = {error, "error message"},
  ?assertEqual(Err, errm_result:assert_error(Err)).

try_macro_empty_test() ->
  ?assertEqual(ok, ?ERRM_TRY(ok)).

try_macro_value_test() ->
  ?assertEqual({ok, "value"}, ?ERRM_TRY({ok, "value"})).

try_macro_error_test() ->
  ?assertError({error, #errm_error{}}, ?ERRM_TRY({error, #errm_error{}})).