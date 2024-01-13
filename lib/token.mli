type token = {
  symbol : string;
  mutable supply : float;
}

val create_token : string -> token
(** [create_token symbol] creates a new token with the given symbol. *)

val mint_tokens : token -> float -> unit
(** [mint_tokens token amount] mints the specified amount of tokens. *)

val transfer_tokens : token -> token -> float -> unit
(** [transfer_tokens sender recipient amount] transfers tokens from sender to
    recipient. *)

val to_string : token -> string
(** [to_string token] generates the string representation of a token *)

val get_supply : token -> float
(** [get_supply token] returns the supply left of a specific token *)
