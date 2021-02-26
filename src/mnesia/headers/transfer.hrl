-record(transfer, {
  id,

  sender_account_id,
  sender_wallet_id,
  sender_old_balance,
  sender_new_balance,

  receiver_account_id,
  receiver_wallet_id,
  receiver_old_balance,
  receiver_new_balance,

  transfer_amount,
  transfer_timestamp}).