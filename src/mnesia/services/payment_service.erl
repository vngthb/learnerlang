-module(payment_service).

-include_lib("../headers/account.hrl").
-include_lib("../headers/wallet.hrl").
-include_lib("../headers/transfer.hrl").

-export([transfer/3]).

transfer(From, To, Amount) ->
  Sending_Account = first(account_mnesia:find_by_email(From)),
  Sending_Wallet = first(wallet_mnesia:find_by_account_identifier(Sending_Account#account.id)),
  Sending_Balance = Sending_Wallet#wallet.balance,
  Sending_Wallet2 = Sending_Wallet#wallet{balance = Sending_Balance - Amount},

  Receiving_Account = first(account_mnesia:find_by_email(To)),
  Receiving_Wallet = first(wallet_mnesia:find_by_account_identifier(Receiving_Account#account.id)),
  Receiving_Balance = Receiving_Wallet#wallet.balance,
  Receiving_Wallet2 = Receiving_Wallet#wallet{balance = Receiving_Balance + Amount},

  Transfer = #transfer{
    sender_account_id = Sending_Account#account.id,
    sender_wallet_id = Sending_Wallet#wallet.id,
    sender_old_balance = Sending_Balance,
    sender_new_balance = Sending_Balance - Amount,
    receiver_account_id = Receiving_Account#account.id,
    receiver_wallet_id = Receiving_Wallet#wallet.id,
    receiver_old_balance = Receiving_Balance,
    receiver_new_balance = Receiving_Balance + Amount,
    transfer_amount = Amount,
    transfer_timestamp = erlang:timestamp()
  },

  Transaction = fun() ->
    Index = length(transfer_mnesia:all()) + 1,
    wallet_mnesia:save([Sending_Wallet2, Receiving_Wallet2]),
    transfer_mnesia:save(Transfer#transfer{id = Index})
             end,

  mnesia:transaction(Transaction).

validate_sender(Sender, Amount) -> ok.

validate_receiver(Receiver) -> ok.

first(List) ->
  lists:nth(1, List).