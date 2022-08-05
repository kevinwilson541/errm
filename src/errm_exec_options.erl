-module(errm_exec_options).

-include("errm_connection.hrl").

%% API
-export([
  new/0,
  new/1,
  transaction/1,
  transaction/2
]).

% @doc Create new errm execution options.
% @returns `errm_exec_options()'
-spec new() -> errm_exec_options().
new() ->
  #errm_exec_options{}.

% @doc Create new errm execution options with transaction set.
% @param TxRef    Transaction to set on errm execution options.
% @returns `errm_exec_options()'
-spec new(TxRef :: errm_transaction()) -> errm_exec_options().
new(TxRef) when is_reference(TxRef) ->
  #errm_exec_options{ transaction=TxRef }.

% @doc Get transaction from errm execution options record, returning `undefined' if not set.
% @param Opts   errm execution option record.
% @returns `errm_transaction() | undefined'
-spec transaction(Opts :: errm_exec_options()) -> errm_transaction() | undefined.
transaction(#errm_exec_options{ transaction=TxRef }) ->
  TxRef.

% @doc Set transaction on errm execution record.
% @param TxRef    Transaction to set on errm execution options.
% @param Opts     Errm execution option record.
% @returns `errm_exec_options()'
-spec transaction(TxRef :: errm_transaction(), Opts :: errm_exec_options()) -> errm_exec_options().
transaction(TxRef, Opts=#errm_exec_options{}) when is_reference(TxRef) ->
  Opts#errm_exec_options{ transaction=TxRef }.