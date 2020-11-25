open Graphics
open State
open Tetromino

(** [change_color tetromino] alters the Graphics color to match the 
    tetromino type. *)
let change_color = function
  | I_block -> set_color cyan;
  | L_block -> set_color blue;
  | J_block -> set_color (rgb 255 165 0);
  | O_block -> set_color yellow;
  | S_block -> set_color green;
  | T_block -> set_color magenta;
  | Z_block -> set_color red

(** [draw_tetromino x y arr] draws a tetromino. *)
let draw_tetrominogrid start_x start_y x y size arr = 
  let grid_width = Array.length arr.(0) * size in
  let grid_height = Array.length arr * size in
  let y_adjstd = Array.length arr - 1 - y in 
  match arr.(y).(x) with 
  | None -> 
    draw_rect (start_x + x * size - grid_width / 2) 
      (start_y + y_adjstd * size - grid_height / 2) size size
  | Some tetromino -> 
    change_color tetromino;
    fill_rect (start_x + x * size - grid_width / 2) 
      (start_y + y_adjstd * size - grid_height / 2) size size; set_color black

(** [draw_grid arr] opens a graph to display a grid with each position filled in
    in black if the corresponding [arr] entry is a 1.
    Note: [arr.(0).(0)] corresponds to the top left position of the grid.
          [arr.(1).(0)] corresponds to one position to the right of top left.
          [arr.(0).(1)] corresponds to one position below top left.
          In general [arr.(y).(x)] corresponds to the position [x] spaces to the
          right and [y] spaces above the bottom left corner.
    Requires: [arr] is a non-empty 2D array of ints. *)
let draw_grid arr =
  let box_size = 20 in
  let start_x = size_x () / 2 in
  let start_y = size_y () / 2 in
  for x = 0 to Array.length arr.(0) - 1 do
    for y = 0 to Array.length arr - 1 do
      draw_tetrominogrid start_x start_y x y box_size arr
    done
  done

(** [draw_score score] draws the score onto an opened game screen. *)
let draw_score score =
  moveto 200 430;
  draw_string ("Score: " ^ string_of_int score)

(** [draw_tetromino x y tetromino] draws [tetromino] centered at the position 
    [x], [y]. *)
let draw_tetromino x y tetromino =
  change_color tetromino;
  begin 
    match tetromino with
    | I_block -> fill_rect (x - 40) (y - 10) 80 20
    | L_block -> fill_rect (x - 30) (y - 20) 60 20; fill_rect (x + 10) y 20 20
    | J_block -> fill_rect (x - 30) (y - 20) 60 20; fill_rect (x - 30) y 20 20
    | O_block -> fill_rect (x - 20) (y - 20) 40 40
    | S_block -> fill_rect (x - 30) (y - 20) 40 20; fill_rect (x - 10) y 40 20
    | T_block -> fill_rect (x - 30) (y - 20) 60 20; fill_rect (x - 10) y 20 20
    | Z_block -> fill_rect (x - 30) y 40 20; fill_rect (x - 10) (y - 20) 40 20
  end;
  set_color black

(** [draw_hold tetromino] draws [teromino] onto an opened game screen if it some
    tetromino or an empty box if it is none. *)
let draw_hold tetromino =
  moveto 72 430;
  draw_string "Held Block";
  draw_rect 50 325 100 100;
  match tetromino with
  | None -> ()
  | Some tetr -> draw_tetromino 100 375 tetr

(** [draw_upcoming tlst] draws the first three elements of [tlst] onto an 
    opened game screen.
    Fails with a helpful message if length [tlst] < 3. *)
let draw_upcoming tlst =
  moveto 456 430;
  draw_string "Upcoming Blocks";
  draw_rect 450 325 100 100;
  draw_rect 450 225 100 100;
  draw_rect 450 125 100 100;
  match tlst with
  | [] -> failwith "Less than 3 upcoming blocks"
  | h :: t -> draw_tetromino 500 375 h;
    match t with
    | [] -> failwith "Less than 3 upcoming blocks"
    | h :: t -> draw_tetromino 500 275 h;
      match t with 
      | [] -> failwith "Less than 3 upcoming blocks"
      | h :: t -> draw_tetromino 500 175 h

let draw_start_screen () =
  open_graph "";
  moveto ((size_x () / 2) - 72) (size_y () - 50);
  draw_string "Welcome to OCaml Tetris!";
  moveto 100 (size_y () - 70);
  draw_string ("Created by Dan Batan, David Hu, Lenhard Thomas, " ^
               "and Rafael Chaves");
  moveto ((size_x () / 2) - 92) ((size_y () * 2 / 3) - 30);
  draw_string "The controls are the following:";
  moveto 80 ((size_y () * 2 / 3) - 50);
  draw_string ("* [a/d] to move the falling tetromino left and right " ^
               "respectively");
  moveto 80 ((size_y () * 2 / 3) - 70);
  draw_string "* [w] to immediately drop the falling tetromino down";
  moveto 80 ((size_y () * 2 / 3) - 90);
  draw_string "* [s] to have the falling tetromino fall faster";
  moveto 80 ((size_y () * 2 / 3) - 110);
  draw_string ("* [k/l] to rotate the falling tetromino counter-clockwise " ^
               "and clockwise respectively");
  moveto 80 ((size_y () * 2 / 3) - 130);
  draw_string "* [space bar] to hold the falling tetromino";
  moveto ((size_x () / 2) - 110) 50;
  draw_string "Press the space bar to start the game"

let draw_game_screen state =
  open_graph "";
  draw_grid (get_grid state);
  draw_score (get_score state);
  draw_hold (get_hold state);
  draw_upcoming (get_upcoming state)

let draw_game_over_screen score =
  open_graph "";
  moveto (size_x () / 2 - 20) (5 * size_y () / 7);
  draw_string "Game Over";
  moveto (size_x () / 2 - 62) (size_y () / 2);
  draw_string ("Your final score was: " ^ string_of_int score);
  moveto (size_x () / 2 - 90) (2 * size_y () / 7);
  draw_string "Press the space bar to play again"