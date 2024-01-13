open Block

type t = Block.t
type chain

val empty : chain
(** [empty] creates an empty blockchain*)

val get_i : chain -> int
(** [get_i chain] retrieves the number of blocks in the chain input*)

val get_chain : chain -> t list
(** [get_chain chain] returns the list of blocks*)

val get_last_block : chain -> t
(** [get_last_block chain] gives the last block in the chain list. Raises: Not
    Found if empty blockchain*)

val add_block : string -> transaction list -> chain -> chain
(** [add_block data transactions] adds a new block to the given block chain with
    the data string and list of transactions provided*)

val to_string : chain -> string
(** [to_string chain] reates a string representation of the blockchain*)

val block_to_json :
  Block.t ->
  [> `Assoc of
     (string
     * [> `Int of int
       | `List of
         [> `Assoc of
            (string
            * [> `Assoc of
                 (string * [> `Float of float | `String of string ]) list
              | `Float of float
              | `String of string
              ])
            list
         ]
         list
       | `String of string
       ])
     list
  ]
(** [block_to_json block] generates a json string representation of the block*)

val to_json : chain -> Yojson.Basic.t
(**[to_json chain] generates a json string representation of the blockchain*)

val send_to_file : string -> Yojson.Basic.t -> unit
(**[send_to_file data] sends the json data representation to the file given by
   the string input*)

val block_of_json : Yojson.Basic.t -> Block.t
(** [block_of_json json] creates a block representation of the json input *)

val chain_of_json : Yojson.Basic.t -> chain
(**[chain_of_json json] creates a blockchain representation of the json input *)

val read_from_json_file : string -> chain
(**[read_from_json_file file] accesses the json file at the string location and
   then creates a blockchain*)
