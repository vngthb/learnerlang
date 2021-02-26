-module(account_mnesia).

-include_lib("../headers/account.hrl").

-export([save/1, update/1, delete/1]).
-export([find_by_id/1, find_by_email/1]).

save(Account) ->
  Inserter = fun() -> mnesia:write(Account) end,
  mnesia:activity(transaction, Inserter).

update(Account) ->
  Acc = find_by_id(Account#account.id),
  if
    Acc /= [] -> save(Account);
    Acc == [] -> exit(no_account_found)
  end.

find_by_id(Id) ->
  Matcher = fun() -> mnesia:match_object(#account{id = Id, _ = '_'}) end,
  mnesia:activity(transaction, Matcher).

find_by_email(Email) ->
  Matcher = fun() -> mnesia:match_object(#account{email = Email, _ = '_'}) end,
  mnesia:activity(transaction, Matcher).

delete(Account) ->
  Eraser = fun() -> mnesia:delete_object(Account) end,
  mnesia:activity(transaction, Eraser).