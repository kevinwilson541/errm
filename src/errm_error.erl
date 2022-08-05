-module(errm_error).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("errm_error.hrl").

%% API
-export([
  new/2,
  new/3,
  code/1,
  message/1,
  description/1,
  driver_error/1
]).

% public methods

% @doc Create new errm error with code `Code' and message `Message'.
% @param Code       Code to set on errm error.
% @param Message    Message to set on errm error.
% @returns `errm_error()'
% @equiv new(Code, Message, undefined)
-spec new(Code :: errm_error_code(), Message :: string()) -> errm_error().
new(Code, Message) when is_list(Message) ->
  new(Code, Message, undefined).

% @doc Create new errm error with code `Code', message `Message', and driver error `DriverError'.
% @param Code           Code to set on errm error.
% @param Message        Message to set on errm error.
% @param DriverError    Error attributed to `errm_gen_driver'.
% @returns `errm_error()'
-spec new(Code :: errm_error_code(), Message :: string(), DriverError :: term()) -> errm_error().
new(Code, Message, DriverError) when is_list(Message) ->
  Description = get_description(Code),
  #errm_error{ code=Code, message=Message, description=Description, driver_error=DriverError }.

% @doc Get code from errm error.
% @param Err    errm error record.
% @returns `errm_error_code()'
-spec code(Err :: errm_error()) -> errm_error_code().
code(#errm_error{ code=Code }) ->
  Code.

% @doc Get message from errm error.
% @param Err    errm error record.
% @returns `string()'
-spec message(Err :: errm_error()) -> string().
message(#errm_error{ message=Msg }) ->
  Msg.

% @doc Get description (derived internally from message) from errm error.
% @param Err    errm error record.
% @returns `string()'
-spec description(Err :: errm_error()) -> string().
description(#errm_error{ description=Desc }) ->
  Desc.

% @doc Get driver error from errm error. Will return `undefined' if no driver error is set on the errm error struct.
% @param Err    errm error record.
% @returns `term() | undefined'
-spec driver_error(Err :: errm_error()) -> term() | undefined.
driver_error(#errm_error{ driver_error=DriverError }) ->
  DriverError.

% private methods

-spec get_description(Code :: errm_error_code()) -> string().
get_description(?ERRM_QUERY_FAILED_ERROR) ->
  "query failed to run in driver";
get_description(?ERRM_CHANGESET_ERROR) ->
  "changeset failed to run";
get_description(?ERRM_TRANSACTION_LEAK_ERROR) ->
  "transactions pending on connection";
get_description(?ERRM_TRANSACTION_NOT_FOUND_ERROR) ->
  "transaction context not found";
get_description(?ERRM_TRANSACTION_MISMATCH_ERROR) ->
  "incorrect transaction context";
get_description(?ERRM_GET_CONNECTION_ERROR) ->
  "failed to acquire connection from pool";
get_description(?ERRM_ASSERT_ERROR) ->
  "assertion error";
get_description(?ERRM_NO_REPO_ERROR) ->
  "no repository found with queried name";
get_description(?ERRM_UNKNOWN_ERROR) ->
  "unknown error";
get_description(_) ->
  "unknown error".

