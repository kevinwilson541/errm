
-type errm_cell() :: undefined | integer() | float() | binary().

-type errm_row() :: list(errm_cell()).

-type errm_query_reply() :: list(errm_row()).