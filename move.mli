(** A single AI move in the game. *)

(** The type [t] represents a move. A move contains the number of rotations,
    the number of moves to the left, and the number of moves to the right.
    At least one of the latter two must be zero in order for the move to be
    unique. *)
type t

(** [initialize rots l r] initializes a move that rotates the piece [rots]
    times, then moves it left [l] times, then right [r] times.
    Requires: either l or r is 0. *)
val initialize : int -> int -> int -> t

(** [num_rotations m] is the number of rotations that the move [m] executes. *)
val num_rotations : t -> int

(** [num_moves_left m] is the number of squares to the left the move [m]
    executes. *)
val num_moves_left : t -> int

(** [num_moves_right m] is the number of squares to the right the move [m]
    executes. *)
val num_moves_right : t -> int

(** [grid_after_move st m] is what the grid in [st] would look like after
    the move [m] is executed. *)
val grid_after_move : State.t -> t -> int array array

(** [execute st m] executes the move [m] in state [st]. *)
val execute : State.t -> t -> unit

(** [move_outcome proj_grid] is a list containing the aggregate height,
    the number of complete lines, the number of holes, and the bumpiness
    for the grid [proj_grid]. *)
val move_outcome : int array array -> float list

(** [get_possible_moves tt grid_width] is a list of the possible moves for the
    tetromino of type [tt] in a grid of width [grid_width]. *)
val get_possible_moves : Tetromino.tetromino_type -> int -> t list