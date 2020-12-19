(** A module for displaying the tetris grid. *)

(** [draw_start_sreen ()] draws a start screen for the tetris game. *)
val draw_start_screen : unit -> unit

(** [draw_game_screen state] draws a game screen based on the given game state
    [state] by drawing a grid of blocks, the score, the currently held block, 
    and the three upcoming blocks to be dropped. *)
val draw_game_screen : State.t -> unit

(** [draw_game_over_screem ()] draws a game over screen for the tetris game
    which displays the total score achieved, final level, and total number of
    lines cleared. *)
val draw_game_over_screen : int -> int -> int -> unit

(**/**)
(* These function are for testing purposes only and therefore are excluded from
   the documentation.*)
val draw_grid : Tetromino.tetromino_type option array array -> unit

val draw_score : int -> unit

val draw_level : int -> unit

val draw_lines_cleared : int -> unit

val draw_combo_multi : int -> unit

val draw_hold : Tetromino.tetromino_type option -> unit

val draw_upcoming : Tetromino.tetromino_type list -> unit
(**/**)