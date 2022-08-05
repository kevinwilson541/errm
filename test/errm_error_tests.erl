-module(errm_error_tests).

-include("errm_error.hrl").
-include_lib("eunit/include/eunit.hrl").

new_test() ->
  Code = ?ERRM_UNKNOWN_ERROR,
  Message = "message goes here",
  Err = errm_error:new(Code, Message),
  ?assert(is_list(Err#errm_error.description)),
  ?assertMatch(#errm_error{ code=Code, message=Message, driver_error=undefined }, Err).

new_driver_error_test() ->
  Code = ?ERRM_UNKNOWN_ERROR,
  Message = "message goes here",
  DriverErr = {error, "some driver error"},
  Err = errm_error:new(Code, Message, DriverErr),
  ?assert(is_list(Err#errm_error.description)),
  ?assertMatch(#errm_error{ code=Code, message=Message, driver_error=DriverErr }, Err).