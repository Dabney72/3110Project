module type Tetromino = sig
  type tetromino

  type t =
    | I_block
    | J_block
    | L_block
    | O_block
    | S_block
    | T_block
    | Z_block

  val generate_list : unit -> t list

end