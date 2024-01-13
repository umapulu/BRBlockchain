open Wallet
open Yojson

type t = Wallet.t list

let empty = []

let find_wallet n lst =
  let rec iterate rem =
    match rem with
    | [] -> None
    | h :: t -> if h.name = n then Some h else iterate t
  in
  iterate lst

let new_wallet n lst =
  match find_wallet n lst with
  | None -> Wallet.create n :: lst
  | Some w -> lst

let get_transactions w = w.transactions

let rec to_string lst =
  match lst with
  | h :: t -> Wallet.to_string h ^ to_string t
  | _ -> ""

let rec wallet_to_json wallet =
  `Assoc
    [
      ("name", `String wallet.name);
      ("balance", `Float wallet.balance);
      ("symbol", `String wallet.tokens.symbol);
      ("supply", `Float wallet.tokens.supply);
    ]

let rec to_json wallet =
  let wallet_json_list = `List (List.map wallet_to_json wallet) in
  Yojson.Basic.from_string (Yojson.Basic.to_string wallet_json_list)

let send_to_file file_path json_string =
  let oc = open_out file_path in
  let () = output_string oc (Yojson.Basic.pretty_to_string json_string) in
  close_out oc

let wallet_of_json wallet =
  let open Yojson.Basic.Util in
  let symbol = wallet |> member "symbol" |> to_string in
  let supply = wallet |> member "supply" |> to_float in
  let token = Token.create_token symbol in
  let _ = Token.mint_tokens token supply in
  {
    name = wallet |> member "name" |> to_string;
    balance = wallet |> member "balance" |> to_float;
    tokens = token;
    transactions = [];
  }

let list_of_json json =
  let open Yojson.Basic.Util in
  let wallets = json |> to_list |> List.map wallet_of_json in
  wallets

let read_from_json_file filename =
  let json_string = Yojson.Basic.from_file filename in
  list_of_json json_string
