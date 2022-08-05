
-define(ERRM_QUERY_FAILED_ERROR, errm_query_failed_error).
-define(ERRM_CHANGESET_ERROR, errm_changeset_error).
-define(ERRM_TRANSACTION_LEAK_ERROR, errm_transaction_leak).
-define(ERRM_TRANSACTION_NOT_FOUND_ERROR, errm_transaction_not_found_error).
-define(ERRM_TRANSACTION_MISMATCH_ERROR, errm_transaction_mismatch).
-define(ERRM_GET_CONNECTION_ERROR, errm_get_connection_error).
-define(ERRM_ASSERT_ERROR, errm_assert_error).
-define(ERRM_UNKNOWN_ERROR, errm_unknown_error).
-define(ERRM_NO_REPO_ERROR, errm_no_repo_error).

-type errm_error_code() :: ?ERRM_QUERY_FAILED_ERROR |
                            ?ERRM_CHANGESET_ERROR |
                            ?ERRM_TRANSACTION_LEAK_ERROR |
                            ?ERRM_TRANSACTION_NOT_FOUND_ERROR |
                            ?ERRM_TRANSACTION_MISMATCH_ERROR |
                            ?ERRM_GET_CONNECTION_ERROR |
                            ?ERRM_ASSERT_ERROR |
                            ?ERRM_UNKNOWN_ERROR |
                            ?ERRM_NO_REPO_ERROR.

-record(errm_error, {
  code :: errm_error_code(),
  message :: string(),
  description :: string(),
  driver_error :: term() | undefined
}).

-type errm_error() :: #errm_error{}.
