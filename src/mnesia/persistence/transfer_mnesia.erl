-module(transfer_mnesia).

-include_lib("../headers/transfer.hrl").
-include_lib("../headers/account.hrl").

-export([save/1]).
-export([
  all/0,
  transfers_between_time_period/2,
  complex_query_test/3,
  most_transfers_sent/0,
  most_transfers_received/0]).

save(Transfer) ->
  Inserter = fun() -> mnesia:write(Transfer) end,
  mnesia:activity(transaction, Inserter).

all() ->
  Matcher = fun() -> mnesia:match_object(#transfer{_ = '_'}) end,
  mnesia:activity(transaction, Matcher).

transfers_between_time_period(Fdt, Tdt) ->
  Matcher = #transfer{
    transfer_timestamp = '$1',
    _ = '_'
  },
  Guards = [
    {'>', '$1', Fdt},
    {'<', '$1', Tdt}
  ],
  Results = ['$_'],
  Patterns = [{Matcher, Guards, Results}],
  PatternMatcher = fun() -> mnesia:select(transfer, Patterns) end,
  mnesia:activity(transaction, PatternMatcher).

complex_query_test(Sid, Rid, Tamnt) ->
  Matcher = #transfer{
    sender_account_id = '$1',
    receiver_account_id = '$2',
    transfer_amount = '$3',
    _ = '_'
  },
  Guards = [
    {'andalso',
      {'==', '$1', Sid},
      {'==', '$2', Rid}
    },
    {'>', '$3', Tamnt}
  ],
  Results = ['$_'],
  Patterns = [{Matcher, Guards, Results}],
  PatternMatcher = fun() -> mnesia:select(transfer, Patterns) end,
  mnesia:activity(transaction, PatternMatcher).

most_transfers_sent() ->
  ok.

most_transfers_received() ->
  ok.