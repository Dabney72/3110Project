(** The type [tetromino] represents a tetris piece. *)
type t

(** [generate_list ()] is a list containing one of each of the seven
    tetrominoes in a random order. *)
val generate_list : unit -> t list

(** [get_width tetromino] is the number of blocks that [tetromino] takes up
    in the x direction. *)
val get_width : t -> int

(** [get_height tetromino] is the number of blocks that [tetromino] takes up
    in the y direction. *)
val get_height : t -> int