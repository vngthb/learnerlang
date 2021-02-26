-module(wallet_mnesia).

-include_lib("../headers/wallet.hrl").

-export([save/1, find_by_account_identifier/1, delete/1]).

save(Wallets) ->
  Inserter = fun(Wallet) -> mnesia:write(Wallet) end,
  mnesia:activity(transaction, fun() -> lists:foreach(Inserter, Wallets) end).

find_by_account_identifier(Account_Identifier) ->
  Matcher = fun() -> mnesia:match_object(#wallet{account_id = Account_Identifier, _ = '_'}) end,
  mnesia:activity(transaction, Matcher).

delete(Wallet) ->
  Eraser = fun() -> mnesia:delete_object(Wallet) end,
  mnesia:activity(transaction, Eraser).