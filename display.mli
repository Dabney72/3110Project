(** [draw_start_sreen ()] creates a start screen for the tetris game. *)
val draw_start_screen : unit -> unit

val draw_grid : int array array -> unit

val draw_score : int -> unit

val draw_hold : Tetromino.tetromino_type option -> unit

val draw_upcoming : Tetromino.tetromino_type list -> unit

(** [draw_game_screen state] creates a game screen based on the given game state
    [state] by drawing a grid of blocks, the score, the currently held block, 
    and the three upcoming blocks to be dropped. *)
val draw_game_screen : State.t -> unit

(** [draw_game_over_screem ()] creates a game over screen for the tetris game 
    and displays the score. *)
val draw_game_over_screen : unit -> unit