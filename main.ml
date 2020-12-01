open Graphics
open Display
open Tetromino
open State
open Unix 

(** [wait_for_space ()] stalls until the space bar is pressed. *)
let rec wait_for_space () =
  if read_key () = ' ' then () else wait_for_space ()

(** [read_input state char] updates game state [state] based on the character
    [char] is or does nothing if [char] is not one of the valid inputs. 
    Valid inputs are w, a, s, d, and space. *)
let read_input state =
  function
  | 'a' -> move_left state
  | 'd' -> move_right state
  | 'w' -> drop state
  | 's' -> fall state
  | 'k' -> rotate_ccw state
  | 'l' -> rotate_cw state
  | ' ' -> hold state
  | _ -> ()

(** [main ()] runs the tetris game. *)
let rec main () =
  (* Initialize game variables and game state and wait for a space bar press to
     start game. *)
  let counter = ref 0 in
  open_graph "";
  draw_start_screen ();
  wait_for_space ();
  let state = initialize () in
  draw_game_screen state;
  (* Main game loop that runs until game over. *)  
  while not (game_over state) do
    sleepf 0.05;
    (* Gets current difficulty level to change how fast the game goes. *)
    let diff = 11 - (get_level state) in
    (* Checks if there is an input from the player. *)
    let () = if key_pressed () then read_input state (read_key ()) else () in
    draw_game_screen state;
    (* Increments counter until it is greater than diff to cause a game update. 
       In the future diff can be changed to speedup the game. *)
    counter := !counter + 1;
    let () = if !counter > diff then fall state else () in
    let () = if !counter > diff then counter := 0 else () in
    ()
  done;
  (* Game over where a space bar press will restart main. *)
  sleepf 1.0;
  draw_game_over_screen (get_score state) (get_level state)
    (get_lines_cleared state);
  wait_for_space ();
  main ()

let () = main ()