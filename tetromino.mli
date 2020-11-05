(** The type [tetromino_type] represents the type of tetromino. Each type
    has a different block composition. *)
type tetromino_type =
  | I_block
  | L_block
  | J_block
  | O_block
  | S_block
  | T_block
  | Z_block

(** The type [t] represents a tetris piece: a composition of four squares
    connected orthogonally. *)
type t

(** [init_tetromino t_type] is the tetromino given by the tetromino type
    [t_type]. *)
val init_tetromino : tetromino_type -> t

(** [create_tetromino comp w] is a tetromino with the composition [comp]
    and width [w]. *)
val create_tetromino : (int * int) list -> int -> t

(** [rotate tetromino] is a new tetromino after [tetromino] is rotated 90
    degrees clockwise. It will shift the tetromino after rotation in order
    to keep it within the same boundary. *)
val rotate : t -> t

(** [generate_list ()] is a list containing one of each of the seven
    tetrominoes in a random order. *)
val generate_list : unit -> t list

(** [get_width tetromino] is the width/height of the square that encloses
    [tetromino]. *)
val get_width : t -> int

(** [get_comp tetromino] is list of the four coordinates of [tetromino]. *)
val get_comp : t -> (int * int) list

(** [to_string tetromino] is a string representation of [tetromino]. *)
val to_string : t -> string
