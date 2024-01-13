open Wallet

type t = Wallet.t list

val empty : t
(** [empty] creates an empty list to hold wallets *)

val new_wallet : string -> t -> t
(** [new_wallet name] creates a new wallet with a given name and appends it to
    the list of wallets *)

val find_wallet : string -> t -> Wallet.t option
(** [find_wallet name list] looks through the list of wallets given a name and
    returns [Some w] where w is a wallet with the same name as the given.
    Returns [None] if no wallet with such name was found*)

val to_string : t -> string
(** [to_string list] returns a string representation of the wallet list*)

val wallet_to_json :
  Wallet.t ->
  [> `Assoc of (string * [> `Float of float | `String of string ]) list ]

val to_json : Wallet.t list -> Yojson.Basic.t
(** [to_json block] generates a json string representation of the wallet list*)

val send_to_file : string -> Yojson.Basic.t -> unit
(**[send_to_file data] sends the json data representation to the file given by
   the string input*)

val wallet_of_json : Yojson.Basic.t -> Wallet.t
(**[chain_of_json json] creates a wallet representation of the json input *)

val list_of_json : Yojson.Basic.t -> t
(**[chain_of_json json] creates a wallet list representation of the json input *)

val read_from_json_file : string -> t
(**[read_from_json_file file] accesses the json file at the string location and
   then creates a wallet list*)
