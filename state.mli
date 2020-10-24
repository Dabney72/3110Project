(** The abstract type representing the game state, containing information
    about:
    - the blocks that are filled in the grid
    - current score
    - list of upcoming blocks
    - block that is currently falling
    - block that is currently being held
*)
type t

(** [initialize ()] creates a new state that contains the following:
    - Empty grid
    - Score set to zero
    - List of upcoming blocks
    - No upcoming blocks
    - No  block being held*)
val initialize : unit -> t

val rotate : t -> unit

val move_left : t -> unit

val move_right : t -> unit

val drop : t -> unit

val hold : t -> unit

val fall : t -> unit

val update_score : t -> unit

(** [grid_width st] is the width of the tetris grid in game state [st]. *)
val grid_width : t -> int

(** [grid_height st] is the height of the tetris grid in game state [st]. *)
val grid_height : t -> int

(** [spawn_tetromino tetromino] mutates the game state by placing [tetromino]
    at the top of the grid. *)
val spawn_tetromino : t -> Tetromino.t -> unit

(** [get_upcoming_blocks st] is a list of the upcoming tetrominoes, with the
    leftmost entries being the ones that will spawn next. *)
val get_upcoming_blocks : t -> Tetromino.t list


