-module(errm_query_options).

-include("errm_connection.hrl").

%% API
-export([
  new/0,
  new/1,
  new/2,
  parameters/1,
  parameters/2,
  transaction/1,
  transaction/2
]).

% @doc Create new errm query options record.
% @returns `errm_query_options()'
% @equiv new([])
-spec new() -> errm_query_options().
new() ->
  new([]).

% @doc Create new errm query options record with query parameter list `Parameters'.
% @param Parameters   List of query parameters to set on errm query options record.
% @returns `errm_query_options()'
-spec new(Parameters :: list()) -> errm_query_options().
new(Parameters) ->
  #errm_query_options{ parameters=Parameters }.

% @doc Create new errm query options record with query parameter list `Parameters' and transaction `TxRef'.
% @param Parameters   List of query parameters to set on errm query options record.
% @param TxRef        Transaction to set on errm query options record.
% @returns `errm_query_options()'
-spec new(Parameters :: list(), TxRef :: errm_transaction()) -> errm_query_options().
new(Parameters, TxRef) ->
  #errm_query_options{ parameters=Parameters, transaction=TxRef }.

% @doc Get query parameters from errm query options record.
% @param Opts   errm query options record.
% @returns `list()'
-spec parameters(Opts :: errm_query_options()) -> list().
parameters(#errm_query_options{ parameters=Params }) ->
  Params.

% @doc Set query parameters on errm query options record.
% @param Params   List of query parameters to set on errm query options record.
% @param Opts     errm query options record to set query parameters on.
% @returns `errm_query_options()'
-spec parameters(Params :: list(), Opts :: errm_query_options()) -> errm_query_options().
parameters(Params, Opts) ->
  Opts#errm_query_options{ parameters=Params }.

% @doc Get transaction from errm query options record, returning `undefined' if no transaction set.
% @param Opts   errm query options record.
% @returns `errm_transaction() | undefined'
-spec transaction(Opts :: errm_query_options()) -> errm_transaction() | undefined.
transaction(#errm_query_options{ transaction=TxRef }) ->
  TxRef.

% @doc Set transaction on errm query options record.
% @param TxRef    Transaction to set on errm query options record.
% @param Opts     errm query options record to set transaction on.
% @returns `errm_query_options()'
-spec transaction(TxRef :: errm_transaction() | undefined, Opts :: errm_query_options()) -> errm_query_options().
transaction(TxRef, Opts) ->
  Opts#errm_query_options{ transaction=TxRef }.
