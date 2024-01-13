open Token

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

val create : string -> t
(** [create name] creates a wallet with a given name and balance of 0. *)

val get_balance : t -> float
(** [get_balance wallet] returns the balance of a given wallet *)

val deposit : t -> float -> unit
(** [deposit wallet] deposits floats units of currency into a wallet. Requires:
    float amount to be non-negative *)

val withdraw : t -> float -> unit
(** [withdraw wallet] withdraws floats units of currency out of a wallet.
    Requires: float amount to be non-negative and at most the current balance *)

val transfer : t -> t -> float -> unit
(** [transfer sender recipient amount] transfers floats units of currency from
    sender to recipient. Requires: float amount to be non-negative and at most
    the current balance of sender *)

val to_string : t -> string
(** [to_string wallet] generates the string representation of a wallet *)

val transfer_token : t -> t -> float -> Token.token -> unit
(** [transfer_token sender recipient amount] transfers floats units of tokens
    from sender to recipient. Requires: float amount to be non-negative and at
    most the current balance of sender *)

val give_token : t -> token -> unit
(** [give_token wallet token] adds tokens to a users wallet *)

val get_last_wallet_transaction : t -> transaction list
(** [get_last_wallet_transaction] returns the last wallet transaction or an
    empty list *)
