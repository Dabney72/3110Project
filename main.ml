open Graphics
open Display
open Tetromino
open State
open Unix 

(** [read_input state char] updates game state [state] based on the character
    [char] is or does nothing if [char] is not one of the valid inputs. 
    Valid inputs are w, a, s, d, and space. *)
let read_input state =
  function
  | 'w' -> rotate state
  | 'a' -> move_left state
  | 'd' -> move_right state
  | 's' -> drop state
  | ' ' -> hold state
  | _ -> ()

(** [main ()] runs the tetris game. *)
let rec main () =
  (* Initialize game variables and game state and wait for any button press to 
     start game. *)
  let counter = ref 0 in
  let diff = ref 5 in
  draw_start_screen ();
  ignore (read_key ());
  let state = initialize () in
  draw_game_screen state;
  (* Main game loop that runs until game over. *)  
  while not (game_over state) do
    sleepf 0.1;
    (* Checks if there is an input from the player. *)
    let () = if key_pressed () then read_input state (read_key ()) else () in
    draw_game_screen state;
    (* Increments counter until it is greater than diff to cause a game update. 
       In the future diff can be changed to speedup the game. *)
    counter := !counter + 1;
    let () = if !counter > !diff then fall state else () in
    let () = if !counter > !diff then draw_game_screen state else () in
    let () = if !counter > !diff then counter := 0 else () in
    ()
  done;
  (* Game over where any button press will restart main. *)
  sleepf 1.0;
  draw_game_over_screen (get_score state);
  ignore (read_key ());
  ignore (read_key ());
  main ()

let () = main ()