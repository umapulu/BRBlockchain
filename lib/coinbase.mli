type transaction = {
  sender : string;
  recipient : string;
  amount : float;
}

val create_coinbase : string -> string -> float -> transaction
(**[create_coinbase sender recipient amount] creates a record of a transaction
   with a sender, recipient, and the amount transacted*)
