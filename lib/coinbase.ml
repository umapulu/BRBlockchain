type transaction = {
  sender : string;
  recipient : string;
  amount : float;
}

let create_coinbase sender recipient amount = { sender; recipient; amount }
