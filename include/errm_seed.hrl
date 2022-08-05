
-record(errm_seed_init, {
  path :: string(),
  schema :: string(),
  table_name :: string(),
  driver :: atom()
}).

-type errm_seed_init() :: #errm_seed_init{}.

-record(errm_seed_options, {
  timeout :: pos_integer()
}).

-type errm_seed_options() :: #errm_seed_options{}.