open Token
open Coinbase

type transaction =
  | CoinTransaction of {
      sender : string;
      recipient : string;
      amount : float;
    }
  | TokenTransaction of {
      sender : string;
      recipient : string;
      amount : float;
      token : Token.token;
    }

type t = {
  name : string;
  mutable balance : float;
  mutable tokens : Token.token;
  mutable transactions : transaction list;
}

exception InsufficientBalance of string

let create name =
  {
    name;
    balance = 0.;
    tokens = Token.create_token "CamelCoin";
    transactions = [];
  }

let get_balance w = w.balance

let deposit w n =
  let temp = w.balance in
  w.balance <- n +. temp;
  w.transactions <-
    CoinTransaction { sender = "External"; recipient = w.name; amount = n }
    :: w.transactions

let give_token (w : t) (token : token) =
  w.tokens <- token;
  w.transactions <-
    TokenTransaction
      {
        sender = "External";
        recipient = w.name;
        amount = token.supply;
        token = w.tokens;
      }
    :: w.transactions

let withdraw w n =
  let temp = w.balance in
  if temp < n then
    raise (InsufficientBalance "\n Insufficient balance in account \n")
  else (
    w.balance <- temp -. n;
    w.transactions <-
      CoinTransaction { sender = w.name; recipient = "External"; amount = n }
      :: w.transactions)

let transfer sender recipient amount =
  let temp = sender.balance in
  if temp < amount then
    raise (InsufficientBalance "\n Insufficient balance in account \n")
  else (
    sender.balance <- temp -. amount;
    recipient.balance <- recipient.balance +. amount;
    let coin_transaction =
      CoinTransaction
        { sender = sender.name; recipient = recipient.name; amount }
    in
    sender.transactions <- coin_transaction :: sender.transactions;
    recipient.transactions <- coin_transaction :: recipient.transactions)

let transfer_token sender recipient amount token =
  if amount >= 0. && sender.tokens.supply >= amount then (
    sender.tokens.supply <- sender.tokens.supply -. amount;
    recipient.tokens.supply <- recipient.tokens.supply +. amount;
    sender.transactions <-
      TokenTransaction
        { sender = sender.name; recipient = recipient.name; amount; token }
      :: sender.transactions;
    recipient.transactions <-
      TokenTransaction
        { sender = sender.name; recipient = recipient.name; amount; token }
      :: recipient.transactions)

let get_last_wallet_transaction (w : t) : transaction list =
  let get_last_transaction = function
    | [] -> []
    | h :: t -> [ h ]
  in
  get_last_transaction w.transactions

let to_string wallet =
  wallet.name ^ "\n"
  ^ string_of_float wallet.balance
  ^ "\n" ^ wallet.tokens.symbol ^ "\n"
  ^ string_of_float wallet.tokens.supply

(* old code

   let deposit w n = let temp = w.balance in w.balance <- n +. temp

   let withdraw w n = let temp = w.balance in w.balance <- (temp -. if temp >= n
   then n else 0.)

   let transfer sender recipient n = let temp = sender.balance in if temp < n
   then () else ( sender.balance <- temp -. n; recipient.balance <-
   recipient.balance +. n) *)
