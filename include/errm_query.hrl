
% from terms

-define(FROM_STMT, "FROM").
-define(FROM(Table), [?FROM_STMT, ?SPC, Table]).

% as terms

-define(AS_STMT, "AS").
-define(AS(A, B), [A, ?SPC, ?AS_STMT, ?SPC, B]).

% where terms

-define(WHERE_STMT, "WHERE").
-define(WHERE(Stmt), [?WHERE_STMT, ?SPC, Stmt]).

% order terms

-define(ORDER_BY_STMT, "ORDER BY").
-define(ORDER_BY(A, B), [?ORDER_BY_STMT, ?SPC, A, ?SPC, B]).

% limit terms

-define(LIMIT_STMT, "LIMIT").
-define(LIMIT(A), [?LIMIT_STMT, ?SPC, A]).

% offset terms

-define(OFFSET_STMT, "OFFSET").
-define(OFFSET(A), [?OFFSET_STMT, ?SPC, A]).

% group by terms

-define(GROUP_BY_STMT, "GROUP BY").
-define(GROUP_BY(Stmts), [?GROUP_BY_STMT, ?SPC, lists:join(", ", Stmts)]).

% join terms

-define(JOIN_STMT, "JOIN").
-define(JOIN(Table), [?JOIN_STMT, ?SPC, Table]).

-define(NATURAL_STMT, "NATURAL").
-define(NATURAL(Stmt), [?NATURAL_STMT, ?SPC, Stmt]).

-define(NATURAL_JOIN_STMT, "NATURAL JOIN").
-define(NATURAL_JOIN(Table), [?NATURAL_JOIN_STMT, ?SPC, Table]).

-define(LEFT_JOIN_STMT, "LEFT JOIN").
-define(LEFT_JOIN(Table), [?LEFT_JOIN_STMT, ?SPC, Table]).

-define(LEFT_OUTER_JOIN_STMT, "LEFT OUTER JOIN").
-define(LEFT_OUTER_JOIN(Table), [?LEFT_OUTER_JOIN_STMT, ?SPC, Table]).

-define(RIGHT_JOIN_STMT, "RIGHT JOIN").
-define(RIGHT_JOIN(Table), [?RIGHT_JOIN_STMT, ?SPC, Table]).

-define(RIGHT_OUTER_JOIN_STMT, "RIGHT OUTER JOIN").
-define(RIGHT_OUTER_JOIN(Table), [?RIGHT_OUTER_JOIN_STMT, ?SPC, Table]).

-define(FULL_JOIN_STMT, "FULL JOIN").
-define(FULL_JOIN(Table), [?FULL_JOIN_STMT, ?SPC, Table]).

-define(FULL_OUTER_JOIN_STMT, "FULL OUTER JOIN").
-define(FULL_OUTER_JOIN(Table), [?FULL_OUTER_JOIN_STMT, ?SPC, Table]).

-define(INNER_JOIN_STMT, "INNER JOIN").
-define(INNER_JOIN(Table), [?INNER_JOIN_STMT, ?SPC, Table]).

-define(CROSS_JOIN_STMT, "CROSS JOIN").
-define(CROSS_JOIN(Table), [?CROSS_JOIN_STMT, ?SPC, Table]).

% select terms

-define(SELECT_STMT, "SELECT").
-define(SELECT_FROM(Fields, Table), [?SELECT_STMT, ?SPC, lists:join(", ", Fields), ?SPC, ?FROM(Table)]).

% insert terms

-define(INSERT_INTO_STMT, "INSERT INTO").
-define(INSERT_INTO(Table), [?INSERT_INTO_STMT, ?SPC, Table]).

-define(VALUES_STMT, "VALUES").
-define(VALUE_TUP(Values), ["(", lists:join(", ", Values), ")"]).
-define(VALUES(ValuesList), [?VALUES_STMT, ?SPC, lists:join(", ", [?VALUE_TUP(Values) || Values <- ValuesList])]).

% update terms

-define(UPDATE_STMT, "UPDATE").
-define(UPDATE(Table), [?UPDATE_STMT, ?SPC, Table]).

-define(SET_STMT, "SET").
-define(SET(Values), [?SET_STMT, ?SPC, lists:join(", ", Values)]).

% delete terms

-define(DELETE_FROM_STMT, "DELETE FROM").
-define(DELETE_FROM(Table), [?DELETE_FROM_STMT, ?SPC, Table]).

% create table terms

-define(CREATE_TABLE_STMT, "CREATE TABLE").
-define(CREATE_TABLE(Table), [?CREATE_TABLE_STMT, ?SPC, Table]).

-define(CREATE_TABLE_IF_NOT_EXISTS_STMT, "CREATE TABLE IF NOT EXISTS").
-define(CREATE_TABLE_IF_NOT_EXISTS(Table), [?CREATE_TABLE_IF_NOT_EXISTS_STMT, ?SPC, Table]).

% alter table terms

-define(ALTER_TABLE_STMT, "ALTER TABLE").
-define(ALTER_TABLE(Table), [?ALTER_TABLE_STMT, ?SPC, Table]).

-define(RENAME_TO_STMT, "RENAME TO").
-define(RENAME_TO(TableAfter), [?RENAME_TO_STMT, ?SPC, TableAfter]).

-define(RENAME_COLUMN_STMT, "RENAME COLUMN").
-define(RENAME_COLUMN(ColumnBefore, ColumnAfter), [?RENAME_COLUMN_STMT, ?SPC, ColumnBefore, " TO ", ColumnAfter]).

-define(ADD_COLUMN_STMT, "ADD COLUMN").
-define(ADD_COLUMN(Column, ColumnDef), [?ADD_COLUMN_STMT, ?SPC, Column, ?SPC, ColumnDef]).

-define(DROP_COLUMN_STMT, "DROP COLUMN").
-define(DROP_COLUMN(Column), [?DROP_COLUMN_STMT, ?SPC, Column]).

% drop table terms

-define(DROP_TABLE_STMT, "DROP TABLE").
-define(DROP_TABLE(Table), [?DROP_TABLE, ?SPC, Table]).

-define(DROP_TABLE_IF_EXISTS_STMT, "DROP TABLE IF EXISTS").
-define(DROP_TABLE_IF_EXISTS(Table), [?DROP_TABLE_IF_EXISTS_STMT, ?SPC, Table]).

% general terms

-define(AND_STMT, "AND").
-define(AND(Stmt), [?AND_STMT, ?SPC, Stmt]).

-define(OR_STMT, "OR").
-define(OR(Stmt), [?OR_STMT, ?SPC, Stmt]).

% operators

-define(EQ_OP, "=").
-define(EQ(A, B), [A, ?EQ_OP, B]).

-define(NE_OP, "<>").
-define(NE(A, B), [A, ?NE_OP, B]).

-define(LT_OP, "<").
-define(LT(A, B), [A, ?LT_OP, B]).

-define(LTE_OP, "<=").
-define(LTE(A, B), [A, ?LTE_OP, B]).

-define(GT_OP, ">").
-define(GT(A, B), [A, ?GT_OP, B]).

-define(GTE_OP, ">=").
-define(GTE(A, B), [A, ?GTE_OP, B]).

-define(IN_OP, "IN").
-define(IN(A, B), [A, ?SPC, ?IN_OP, ?SPC, B]).

-define(NOT_OP, "NOT").
-define(NOT(A), [?NOT_OP, ?SPC, A]).

-define(NOT_IN(A, B), [A, ?NOT_OP, ?SPC, ?IN_OP, ?SPC, B]).

-define(BETWEEN_STMT, "BETWEEN").
-define(BETWEEN(A, B, C), [A, ?SPC, ?BETWEEN_STMT, B, ?SPC, ?AND_STMT, C]).

% statements

-define(SPC, " ").

-define(STMT_END, ";").
-define(STMT(Statement), lists:join(?SPC, Statement)).

-define(STMT_F(Statement), iolist_to_binary([lists:join(?SPC, Statement), ?STMT_END])).

-define(EXPR(Expr), ["(", Expr, ")"]).