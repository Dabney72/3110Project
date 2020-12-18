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
    - No block being held
    - Difficulty level set to one
    - No lines cleared
    - Game over set to false
    - Use hold set to false *)
val initialize : ?first_block: Tetromino.tetromino_type option -> 
  ?auto_spawn: bool -> unit -> t

(** [get_falling_block st] is the type of tetromino that is falling. *)
val get_falling_block : t -> Tetromino.tetromino_type

(** [get_grid st] is a 2d array that contains the placements of the blocks. The
    grid is in row major so grid.(0) is the first row and grid.(0).(1) is the
    first row, second column. *)
val get_grid : t -> Tetromino.tetromino_type option array array 

(** [get_score st] is an integer representing the game's current score. *)
val get_score : t -> int

(** [get_hold st] is the Tetromino block that is currently being held, [None] 
    if no block is being held. *)
val get_hold : t -> Tetromino.tetromino_type option

(** [get_upcoming st] is a list of the upcoming tetrominoes, with the
    leftmost entries being the ones that will spawn next. *)
val get_upcoming : t -> Tetromino.tetromino_type list

(** [get_level st] is the current difficulty level of the tetris game. *)
val get_level : t -> int

(** [get_lines_cleared st] is the number of lines cleared of the tetris game. *)
val get_lines_cleared : t -> int

(** [get_combo_multi st] is the combo multiplier of the tetris game. *)
val get_combo_multi : t -> int

(** [game_over st] is whether the game is over in [st]. *)
val game_over : t -> bool

(** [use_hold st] is whether the hold function can be used in [st] *)
val use_hold : t -> bool

(** [move_left st] moves the falling block to the left by one. *)
val move_left : t -> unit

(** [move_right st] moves the falling block to the right by one. *)
val move_right : t -> unit

(** [rotate_cw st] rotates the currently falling tetromino 90 degrees
    clockwise. *)
val rotate_cw : t -> unit

(** [rotate_ccw st] rotates the currently falling tetromino 90 degrees
    counterclockwise. *)
val rotate_ccw : t -> unit

(** [drop ?line_clears ?auto_respawn st] instantaneously drops the currently
    falling tetromino to the bottom of the grid. If there is a block in the way,
    it will drop it on top of that block.
    The optional argument [auto_respawn] is whether the user would like
    the next block to be spawned automatically after the block is placed.
    Defaulted to true.
    The optional argument [line_clears] is whether lines should be cleared
    if the drop results in a full row. Defaulted to true. *)
val drop : ?line_clears: bool -> ?auto_respawn : bool -> t -> unit

(** [hold st] puts the currently falling tetromino to the side for later use.
    If there is already a block being held, that block will spawn at the top. If
    not, the next block in the list of upcoming blocks in [st] will spawn. *)
val hold : t -> unit

(** [fall st] moves the falling block down by one.
    The optional argument [auto_respawn] is whether the user would like
    the next block to be spawned automatically after the block is placed.
    Its default value is true. *)
val fall : ?auto_respawn : bool -> t -> unit

(** [grid_width st] is the width of the tetris grid in game state [st]. *)
val grid_width : t -> int

(** [grid_height st] is the height of the tetris grid in game state [st]. *)
val grid_height : t -> int

(** [copy_grid_int st] is a copy of the current grid in [st], represented with
    an integer matrix. *)
val copy_grid_int : t -> int array array

(** [copy_grid_and_falling st] is a game state with the same grid and falling
    block as [st] while the other fields are the initizlized values. *)
val copy_grid_and_falling : t -> t

(** [spawn_tetromino tetromino] mutates the game state by placing [tetromino]
    at the top of the grid. *)
val spawn_tetromino : Tetromino.tetromino_type -> t -> unit

(** [spawn_next st] spawns the next tetromino from the list of upcoming blocks
    maintained by [st]. *)
val spawn_next : t -> unit

(**/**)
(* These function are for testing purposes only and therefore are excluded from
   the documentation. *)

val increment_lines_cleared : t -> int -> unit

val update_grid : Tetromino.tetromino_type option array array -> t -> t
(**/**)

