
-define(ERRM_TRY(Stmt), errm_result:assert(Stmt)).

-define(ERRM_TRY_OK(Stmt), errm_result:assert_ok(Stmt)).