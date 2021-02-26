-module(payapp).

-behavior(application).

-include_lib("headers/account.hrl").
-include_lib("headers/wallet.hrl").
-include_lib("headers/transfer.hrl").

-export([install/1]).
-export([start/2, stop/1]).
-export([load_data/0]).

install(Nodes) ->
  ok = mnesia:create_schema(Nodes),
  application:start(mnesia),
  mnesia:create_table(account,
    [{attributes, record_info(fields, account)},
      {access_mode, read_write},
      {disc_copies, Nodes}]),
  mnesia:create_table(wallet,
    [{attributes, record_info(fields, wallet)},
      {access_mode, read_write},
      {disc_copies, Nodes}]),
  mnesia:create_table(transfer,
    [{record_name, transfer},
      {attributes, record_info(fields, transfer)},
      {access_mode, read_write},
      {disc_copies, Nodes},
      {type, set}]).
%%  application:stop(mnesia).

start(normal, []) ->
  ok.

stop(_) ->
  ok.

load_data() ->
  Account1 = #account{id = 1, email = "account1@email.com", password = "password", status = active},
  Account2 = #account{id = 2, email = "account2@email.com", password = "password", status = suspended},
  Account3 = #account{id = 3, email = "account3@email.com", password = "password", status = active},
  Account4 = #account{id = 4, email = "account4@email.com", password = "password", status = active},

  account_mnesia:save(Account1),
  account_mnesia:save(Account2),
  account_mnesia:save(Account3),
  account_mnesia:save(Account4),

  Wallet1 = #wallet{account_id = Account1#account.id, id = 1, balance = 4000, status = active},
  Wallet2 = #wallet{account_id = Account2#account.id, id = 2, balance = 2000, status = suspended},
  Wallet3 = #wallet{account_id = Account3#account.id, id = 3, balance = 0, status = active},
  Wallet4 = #wallet{account_id = Account4#account.id, id = 4, balance = 1000, status = active},

  wallet_mnesia:save([Wallet1, Wallet2, Wallet3, Wallet4]),

  Transfer1 = #transfer{
    sender_account_id = 1,
    sender_wallet_id = 1,
    sender_old_balance = 5000,
    sender_new_balance = 4000,

    receiver_account_id = 4,
    receiver_wallet_id = 4,
    receiver_old_balance = 0,
    receiver_new_balance = 1000,

    transfer_amount = 1000,
    transfer_timestamp = erlang:timestamp()},

  transfer_mnesia:save(Transfer1).