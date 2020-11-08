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

(** [get_grid st] is a 2d array that contains the placements of the blocks. The
    grid is in row major so grid.(0) is the first row and grid.(0).(1) is the
    first row, second column. *)
val get_grid : t -> int array array 

(** [get_score st] is an integer representing the game's current score. *)
val get_score : t -> int

(** [get_hold st] is the Tetromino block that is currently being held, [None] 
    if no block is being held. *)
val get_hold : t -> Tetromino.tetromino_type option

(** [get_upcoming st] is a list of the upcoming tetrominoes, with the
    leftmost entries being the ones that will spawn next. *)
val get_upcoming : t -> Tetromino.tetromino_type list

(** [move_left st] mutates the game state by moving the falling block to the 
    left by one. *)
val move_left : t -> unit

(** [move_right st] mutates the game state by moving the falling block to the
    right by one. *)
val move_right : t -> unit

val rotate : t -> unit

val drop : t -> unit

val hold : t -> unit

(** [fall st] mutates the game state by moving the falling block down by one. *)
val fall : t -> unit

val update_score : t -> unit

(** [grid_width st] is the width of the tetris grid in game state [st]. *)
val grid_width : t -> int

(** [grid_height st] is the height of the tetris grid in game state [st]. *)
val grid_height : t -> int

(** [spawn_tetromino tetromino] mutates the game state by placing [tetromino]
    at the top of the grid. *)
val spawn_tetromino : Tetromino.t -> t -> unit

(** [spawn_next st] spawns the next tetromino from the list of upcoming blocks
    maintained by [st]. *)
val spawn_next : t -> unit

