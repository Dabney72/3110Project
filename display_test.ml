(** Test file for Display.ml file.
    Due to the nature of the display functions these tests can only be used by 
    opening this file in utop and running the test functions in order to check 
    by eye that the produced drawings make sense. *)

open Display
open Tetromino

(** [test_start ()] opens the start screen. *)
let test_start () =
  Graphics.open_graph ""; draw_start_screen ()

(** [test_grid grd] opens an empty display and then draws an empty grid with the 
    same dimensions as [grd] and with spots filled in if the corresponding entry 
    in [grd] is a 1 (see draw_grid documentation for more info).
    Requires: [grd] has type int array array. *)
let test_grid grd = 
  Graphics.open_graph ""; draw_grid grd

(** [test_score scr] opens an empty display and then draws the score as being 
    [scr]. 
    Requires: [scr] has type int. *)
let test_score scr =
  Graphics.open_graph ""; draw_score scr

(** [test_level lvl] opens an empty display and then draws the difficulty level
    as being [lvl]. 
    Requires: [lvl] has type int. *)
let test_level lvl =
  Graphics.open_graph ""; draw_level lvl

(** [test_lines_cleared lines] opens an empty display and then draws the number
    of lines cleared as being [lines]. 
    Requires: [lines] has type int. *)
let test_lines lines =
  Graphics.open_graph ""; draw_level lines

(** [test_held_block tetr] opens an empty display and then draws [tetr] as the 
    held block. 
    Requires: [tetr] has type tetromino_type. *)
let test_held tetr =
  Graphics.open_graph ""; draw_hold (Some tetr)

(** [test_upcoming tlst] opens an empty display and then draws the first three 
    elements of [tlst] as the upcoming blocks. If length [tlst] < 3 then a 
    helpful message is printed instead. 
    Requires: [tlst] has type tetromino_type list. *)
let test_upcoming tlst =
  Graphics.open_graph "";
  try draw_upcoming tlst with
  | (Failure _) -> 
    print_string ("Less than three elements in provided list. In normal " ^ 
                  "execution this would raise an exception.")

(** [test_init_game_state ()] initializes a game state and then draws it to test
    the draw_game_screen function. This has the added bonus of testing the 
    upcoming block draw function with the randomized upcoming block list as well
    as a random block being spawned at the top of the grid. *)
let test_init_game_state () =
  Graphics.open_graph ""; draw_game_screen (State.initialize ())

(** [test_game_over scr lvl lines] opens the game over screen using [scr] as the
    total score to display, [lvl] as the final level, and [lines] as the total
    number of lines cleared.
    Requires: [scr], [lvl], and [lines] have type int. *)
let test_game_over scr lvl lines = 
  Graphics.open_graph ""; draw_game_over_screen scr lvl lines