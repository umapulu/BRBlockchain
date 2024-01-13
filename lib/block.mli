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

val generate_hash : string -> string
(** [generate_hash data] takes an input string and generates a hash using the
    sha 256 hash algorithm *)

val count_zeros : string -> int -> bool
(** [count_zeros data] takes in a string and a number of zeros to count and
    returns whether or not there is that number of zeroes at the beginning of
    the string *)

val mine_block : t -> int -> t * string
(** [mine_block block difficulty] generates a block and corresponding hash that
    fufills a given difficulty, which is the number 0's at the beginning*)

val validate_block : string -> int -> bool
(** [validate_block block difficulty] checks if a block fufills a given
    difficulty level*)

val make_block : string -> transaction list -> t
(** [make_block data transactions] generates a block object given a string of
    data and list of transactions. This is the first block in a block chain*)

val make_next_block : string -> t option -> transaction list -> t
(** [make_next_block data previous transactions] generates a block object given
    a string of data, previous block, and list of transactions. If there is no
    previous block, then this creates a new block chain*)

val to_string : t -> string
(** [to_string block] generates a string representation of the block object*)

val gather_transactions : t -> string
(** [gather_transactions block] generates a string representation of the
    transactions of a given block*)

val transactions_to_string : transaction list -> string
