
-record(errm_migrate_init, {
  path :: string(),
  schema :: string(),
  table_name :: string(),
  driver :: atom()
}).

-type errm_migrate_init() :: #errm_migrate_init{}.

-record(errm_migrate_options, {
  timeout :: pos_integer()
}).

-type errm_migrate_options() :: #errm_migrate_options{}.