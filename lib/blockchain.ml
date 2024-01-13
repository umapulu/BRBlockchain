open Block
open Yojson.Basic
open Wallet

type t = Block.t
type chain = t list * int
type transaction = Wallet.transaction

let empty : chain = ([], 0)

(* let get_last_hash chain = match chain with | [], len -> "" | lst, len ->
   (List.nth lst (List.length lst - 1)).hash *)

let get_i (lst, i) = i
let get_chain (lst, i) = lst

let get_last_block chain =
  match chain with
  | [], len -> raise Not_found
  | lst, len -> List.nth lst (List.length lst - 1)

(* let add_block data chain = match chain with | [], len -> ([ Block.make_block
   data ], len + 1) | lst, len -> ( lst @ [ Block.make_next_block data (Some
   (get_last_block chain)) ], len + 1 ) *)
let add_block data transactions chain =
  match chain with
  | [], len -> ([ Block.make_block data transactions ], len + 1)
  | lst, len ->
      let last_block = List.hd (List.rev lst) in
      let new_block =
        Block.make_next_block data (Some last_block) transactions
      in
      (lst @ [ new_block ], len + 1)

let rec to_string (chain : chain) =
  match chain with
  | [], length -> ""
  | h :: t, length -> Block.to_string h ^ "\n" ^ to_string (t, length - 1)

(* let rec to_string chain = match chain with | [], length -> "" | h :: t,
   length -> Block.to_string h ^ "\n" ^ to_string (t, length - 1) *)
let token_to_json (token : Token.token) =
  `Assoc [ ("symbol", `String token.symbol); ("supply", `Float token.supply) ]

let transaction_to_json transaction =
  match transaction with
  | CoinTransaction { sender; recipient; amount } ->
      `Assoc
        [
          ("sender", `String sender);
          ("recipient", `String recipient);
          ("amount", `Float amount);
        ]
  | TokenTransaction { sender; recipient; amount; token } ->
      `Assoc
        [
          ("sender", `String sender);
          ("recipient", `String recipient);
          ("amount", `Float amount);
          ("token", token_to_json token);
        ]

let rec block_to_json block =
  `Assoc
    [
      ("data", `String block.data);
      ("hash", `String block.hash);
      ("nonce", `Int block.nonce);
      ("index", `Int block.index);
      ("previous_hash", `String block.previous_hash);
      ("timestamp", `Int block.timestamp);
      ("transactions", `List (List.map transaction_to_json block.transactions));
    ]

let rec to_json chain =
  let block_json_list = `List (List.map block_to_json (fst chain)) in
  Yojson.Basic.from_string (Yojson.Basic.to_string block_json_list)

let send_to_file file_path json_string =
  let oc = open_out file_path in
  let () = output_string oc (Yojson.Basic.pretty_to_string json_string) in
  close_out oc

let transaction_of_json json =
  let open Yojson.Basic.Util in
  let sender = json |> member "sender" |> to_string in
  let recipient = json |> member "recipient" |> to_string in
  let amount = json |> member "amount" |> to_float in
  match member "token" json with
  | `Null -> CoinTransaction { sender; recipient; amount }
  | token_json ->
      let token = Token.create_token (to_string token_json) in
      TokenTransaction { sender; recipient; amount; token }

let block_of_json json =
  let open Yojson.Basic.Util in
  {
    data = json |> member "data" |> to_string;
    hash = json |> member "hash" |> to_string;
    nonce = json |> member "nonce" |> to_int;
    index = json |> member "index" |> to_int;
    previous_hash = json |> member "previous_hash" |> to_string;
    timestamp = json |> member "timestamp" |> to_int;
    transactions = [];
  }

let chain_of_json json =
  let open Yojson.Basic.Util in
  let blocks = json |> to_list |> List.map block_of_json in
  (blocks, List.length blocks)

let read_from_json_file file =
  let json_string = Yojson.Basic.from_file file in
  chain_of_json json_string
