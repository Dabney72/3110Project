(** The abstract type representing the game state, containing information
    about:
    - the blocks that are filled in the grid
    - current score
    - list of upcoming blocks
    - block that is currently falling
    - block that is currently being held
*)
type t

val rotate : t -> unit

val move_left : t -> unit

val move_right : t -> unit

val drop : t -> unit

val hold : t -> unit

val fall : t -> unit

val update_score : t -> unit

(* val spawn_tetromino : Tetromino.t -> unit

val get_upcoming_blocks : unit -> Tetromino.t list *)


