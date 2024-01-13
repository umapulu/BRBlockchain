open Digest
open Coinbase
open Wallet

type transaction = Wallet.transaction

type t = {
  data : string;
  hash : string;
  nonce : int;
  index : int;
  previous_hash : string;
  timestamp : int;
  mutable transactions : transaction list;
}

(* let find_nonce data = data *)
let generate_hash data = Digest.string data |> Digest.to_hex

let transactions_to_string transactions =
  List.fold_left
    (fun acc transactions ->
      match transactions with
      | CoinTransaction t ->
          acc ^ " " ^ t.sender ^ " -> " ^ t.recipient ^ " "
          ^ string_of_float t.amount ^ "\n"
      | TokenTransaction t ->
          (acc ^ " " ^ t.sender ^ " -> " ^ t.recipient ^ " "
         ^ string_of_float t.amount ^ " " ^ t.token.symbol)
          ^ "\n")
    "" transactions

let gather_transactions block = transactions_to_string block.transactions

let count_zeros hash difficulty =
  let rec starts_with_zeros str count =
    if count < 0 then true
    else if String.get str count = '0' then starts_with_zeros str (count - 1)
    else false
  in
  starts_with_zeros hash difficulty

let rec mine_block block difficulty =
  let rec mine_with_nonce nonce =
    let candidate_block = { block with nonce } in
    let candidate_data =
      candidate_block.data
      ^ string_of_int candidate_block.nonce
      ^ candidate_block.previous_hash
      ^ gather_transactions candidate_block
    in
    let candidate_hash = generate_hash candidate_data in
    if count_zeros candidate_hash difficulty then
      ({ candidate_block with hash = candidate_hash }, candidate_hash)
    else mine_with_nonce (nonce + 1)
  in
  mine_with_nonce 0

let validate_block block difficulty =
  let calculated_hash = generate_hash block in
  if count_zeros calculated_hash difficulty then true else false

let make_next_block (data : string) (previous : t option)
    (coinbase : transaction list) : t =
  match previous with
  | None ->
      let new_block =
        {
          data;
          hash = generate_hash data;
          nonce = 1;
          index = 0;
          previous_hash = "";
          timestamp = 0;
          transactions = coinbase;
        }
      in
      let block, h = mine_block new_block 3 in
      block
  | Some t ->
      let new_block =
        {
          data;
          hash = generate_hash (data ^ t.hash);
          nonce = 1;
          index = t.index + 1;
          previous_hash = t.hash;
          timestamp = 0;
          transactions = coinbase;
        }
      in
      let block, h = mine_block new_block 3 in
      block

(* let make_next_block (data : string) (previous : t option) : t = match
   previous with | None -> let new_block = { data; hash = generate_hash data;
   nonce = 1; index = 0; previous_hash = ""; timestamp = 0; } in let block, h =
   mine_block new_block 3 in block | Some t -> let new_block = { data; hash =
   generate_hash (data ^ t.hash); nonce = 1; index = t.index + 1; previous_hash
   = t.hash; timestamp = 0; } in let block, h = mine_block new_block 3 in
   block *)

let make_block (data : string) (coinbase : transaction list) : t =
  make_next_block data None coinbase

let to_string (block : t) : string =
  " Block: " ^ string_of_int block.index ^ "\n  Hash: " ^ block.hash
  ^ "\n  Nonce: " ^ string_of_int block.nonce ^ "\n  Data: " ^ block.data
  ^ "\n  Previous hash: " ^ block.previous_hash ^ "\n  Time stamp: "
  ^ string_of_int block.timestamp
  ^ "\n"
