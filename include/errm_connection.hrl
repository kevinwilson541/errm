
-record(errm_connection_init, { module :: module(), args :: [term()] }).

-type errm_connection_init() :: #errm_connection_init{}.

-type errm_transaction() :: reference().

-record(errm_connection_state, { module :: module(), driver :: term(), transaction_stack :: [errm_transaction()] }).

-type errm_connection_state() :: #errm_connection_state{}.

-type errm_connection() :: pid().

-record(errm_query_options, {
  transaction :: errm_transaction() | undefined,
  parameters :: list()
}).

-type errm_query_options() :: #errm_query_options{}.

-record(errm_exec_options, {
  transaction :: errm_transaction() | undefined
}).

-type errm_exec_options() :: #errm_exec_options{}.

-record(errm_get_conn_options, {
  conn_type :: main | replica,
  timeout :: pos_integer()
}).

-type errm_get_conn_options() :: #errm_get_conn_options{}.

-record(errm_release_conn_options, {
  timeout :: pos_integer()
}).

-type errm_release_conn_options() :: #errm_release_conn_options{}.

-record(errm_transaction_options, {
  isolation_level :: iodata(),
  timeout :: pos_integer()
}).

-type errm_transaction_options() :: #errm_transaction_options{}.

-define(ERRM_CONN_SAVEPOINT, "errm_savepoint_").

-define(ERRM_CONN_DEFAULT_TIMEOUT, 5000).