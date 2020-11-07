open Graphics
open State
open Tetromino

(** [draw_grid arr] opens a graph to display a grid with each position filled in
    in black if the corresponding [arr] entry is a 1.
    Note: [arr.(0).(0)] corresponds to the bottom left position of the grid.
          [arr.(1).(0)] corresponds to one position to the right of bottom left.
          [arr.(0).(1)] corresponds to one position above bottom left.
          In general [arr.(y).(x)] corresponds to the position [x] spaces to the
          right and [y] spaces above the bottom left corner.
    Requires: [arr] is a non-empty 2D array of ints. *)
let draw_grid arr =
  let box_size = 20 in
  let start_x = size_x () / 2 in
  let start_y = size_y () / 2 in
  let grid_width = Array.length arr.(0) * box_size in
  let grid_height = Array.length arr * box_size in
  for x = 0 to Array.length arr - 1 do
    for y = 0 to Array.length arr.(0) - 1 do
      if arr.(x).(y) = 1 
      then fill_rect (start_x + x * box_size - grid_width / 2) 
          (start_y + y * box_size - grid_height / 2) box_size box_size
      else draw_rect (start_x + x * box_size - grid_width / 2) 
          (start_y + y * box_size - grid_height / 2) box_size box_size
    done
  done

(** [draw_score score] draws the score onto an opened game screen. *)
let draw_score score =
  moveto 200 430;
  draw_string ("Score: " ^ string_of_int score)

(** [draw_tetromino x y tetromino] draws [tetromino] centered at the position 
    [x], [y]. *)
let draw_tetromino x y tetromino =
  match tetromino with
  | I_block -> fill_rect (x - 10) (y - 40) 20 80
  | L_block -> ()
  | J_block -> ()
  | O_block -> ()
  | S_block -> ()
  | T_block -> ()
  | Z_block -> ()

(** [draw_hold tetromino] draws [teromino] onto an opened game screen if it some
    tetromino or an empty box if it is none. *)
let draw_hold tetromino =
  moveto 72 430;
  draw_string "Held Block";
  draw_rect 50 325 100 100;
  match tetromino with
  | None -> ()
  | Some tetr -> draw_tetromino 100 375 tetr

(** [draw_upcoming tlist] draws the first three elements of [tlist] onto an 
    opened game screen. For each element if it is some tetromino then that 
    tetromino is drawn and if it is none than nothing is drawn. *)
let draw_upcoming tlst =
  moveto 456 430;
  draw_string "Upcoming Blocks";
  draw_rect 450 325 100 100;
  draw_rect 450 225 100 100;
  draw_rect 450 125 100 100

let draw_start_screen () =
  open_graph "";
  moveto ((size_x () / 2) - 72) (size_y () - 50);
  draw_string "Welcome to OCaml Tetris!";
  moveto 80 (size_y () - 70);
  draw_string "game created by Dan Batan, David Hu, Lenhard Thomas, and Rafael Chaves";
  moveto ((size_x () / 2) - 90) ((size_y () * 2 / 3) - 40);
  draw_string "the controls are the following:";
  moveto 120 ((size_y () * 2 / 3) - 60);
  draw_string "* [a/d] to move the falling tetromino left and right respectively";
  moveto 120 ((size_y () * 2 / 3) - 80);
  draw_string "* [w] to rotate the falling tetromino clockwise";
  moveto 120 ((size_y () * 2 / 3) - 100);
  draw_string "* [s] to immediately drop the falling tetromino down";
  moveto 120 ((size_y () * 2 / 3) - 120);
  draw_string "* [space bar] to hold the falling tetromino";
  moveto ((size_x () / 2) - 115) 50;
  draw_string "push the [space bar] to start the game"

let draw_game_screen state =
  open_graph "";
  draw_grid (get_grid state);
  draw_score (get_score state);
  draw_hold (get_hold state);
  draw_upcoming (get_upcoming state)

let draw_game_over_screen () =
  failwith "Unimplemented"