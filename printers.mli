(** A module of custom printers for the Tetris game. *)

(** [pp_array pp_elt arr] pretty prints an array [arr], using [pp_elt] to
    print its elements. *)
val pp_array : ('a -> string) -> 'a array -> string

(** [pp_int_matrix m] pretty prints an int matrix [m]. *)
val pp_int_matrix : int array array -> string

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
val pp_list : ('a -> string) -> 'a list -> string

(** [pp_tetromino t] pretty prints the tetromino [t]. *)
val pp_tetromino : Tetromino.t -> string
