
-record(errm_changeset_init, {
  path :: string(),
  schema :: string(),
  table_name :: string(),
  driver :: atom()
}).

-type errm_changeset_init() :: #errm_changeset_init{}.

-record(errm_changeset_state, {
  changesets :: ets:table(),
  schema :: string(),
  table_name :: string(),
  driver :: atom()
}).

-type errm_changeset_state() :: #errm_changeset_state{}.

-record(errm_changeset_options, {
  transaction :: boolean(),
  timeout :: non_neg_integer()
}).

-type errm_changeset_options() :: #errm_changeset_options{}.

-type errm_changeset_status() :: up | down | unknown.

-record(errm_changeset, {
  name :: string(),
  up_sql :: string(),
  up_options :: errm_changeset_options(),
  down_sql :: string(),
  down_options :: errm_changeset_options(),
  status :: errm_changeset_status()
}).

-type errm_changeset() :: #errm_changeset{}.

-define(ERRM_CHANGESET_TABLE, errm_changeset_table).

-define(ERRM_CHANGESET_DEFAULT_TIMEOUT, 120000).