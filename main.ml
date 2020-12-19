open Graphics
open Display
open Tetromino
open State
open Strategy
open Unix  
open Str
open Printers

(**[read_lines name] reads lines from a [name] text file to a int int list*)
let read_lines name =
  let file = open_in name in
  let try_read () =
    try Some (input_line file) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some line -> begin
        if Str.string_match (Str.regexp "[0-9]+") line 0 = true then 
          loop (List.map int_of_string (Str.split (Str.regexp "[^0-9]+") line) 
                :: acc)
        else 
          loop acc
      end
    | None -> close_in file; List.rev acc in
  loop []

(**[mycompare lst1 lst2] is a comparison function comparing the scores for 
   sorting purposes *)
let mycompare lst1 lst2 =
  if List.length lst1 = List.length lst2 then 
    begin
      match lst1,lst2 with
      |[_;_;_;_;_;score1],[_;_;_;_;_;score2]-> if score1 > score2 then -1 else 1
      | _,_ -> failwith "Comparison Error"
    end
  else 
    failwith "Lengths are not the same"

(**[print_header file string] writes out the headers for the highscore.txt file *)
let print_header file string = 
  Printf.fprintf file "%s" string

(**[print_line_score file mon dy yr hr min score] writes out a line containing
   the month,day,year,hour,minute, and score to the highscore.txt file *)
let print_line_score file mon dy yr hr min score =
  Printf.fprintf file "%d %5d %5d %5d %10d %11d \n" mon dy yr hr min score

(** [rec write_multiple_lines file list] writes each score line in the list
    to the highscore.txtfile and closes the file once all the scores has been 
    written. Used if there are multiple scores to be written to the file.
*)
let rec write_multiple_lines file list= 
  match list with 
  | [] -> close_out file;
  | [month;day;year;hour;minute;score_num]::t -> 
    begin
      print_line_score file month day year hour minute score_num; 
      write_multiple_lines file t; 
    end
  | _ -> failwith "Error"

(** [write_highscore_file ai_indicator mon dy yr hr min score] produces a text
    file containing all the scores the player has sorted by score in descending order.*)
let write_highscore_file ai_indicator mon dy yr hr min score = 
  if Sys.file_exists "highscore.txt" = false && !ai_indicator = false then 
    begin
      let oc = open_out "highscore.txt" in
      print_header oc "Month Day Year Hour(24-hr) Minute     Score\n";
      print_line_score oc mon dy yr hr min score;
      close_out oc; 
    end
  else if Sys.file_exists "highscore.txt" = true && !ai_indicator = false then
    begin
      let nc = read_lines "highscore.txt" in 
      let oc = open_out "highscore.txt" in
      print_header oc "Month Day Year Hour(24-hr) Minute     Score\n";
      write_multiple_lines oc (List.sort mycompare ([mon;dy;yr;hr;min;score]::nc))
    end

(** [wait_for_space ()] stalls until the space bar is pressed. *)
let rec wait_for_space () =
  if read_key () = ' ' then () else wait_for_space ()

(** [wait_for_space_or_a ai] stalls until either the space bar or 'a' key is 
    pressed. If space is pressed then [ai] is set to false, but if 'a' is 
    pressed then [ai] is set to true. *)
let rec wait_for_space_or_a ai =
  match read_key () with
  | ' ' -> ai := false
  | 'a' -> ai := true
  | _ -> wait_for_space_or_a ai

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

(** [-0.989144479989705339; 0.00239436400334614146; -0.0187718879248342727;
    -0.14572261646927781] *)
let ai_strategy = 
  Strategy.init_with_weights (-0.989144479989705339) 0.00239436400334614146
    (-0.0187718879248342727) (-0.14572261646927781)

let play_game_user state = 
  if key_pressed () then read_input state (read_key ()) else ()

let play_game_ai state =
  move_next_piece ai_strategy state
(* Strategies.generation ai_strategy |> ignore *)

(** [main ()] runs the tetris game. *)
let rec main () =
  print_endline (pp_list string_of_float (to_list ai_strategy));
  (* Initialize game variables and game state and wait for a space bar press to
     start game. *)
  let ai = ref false in
  let counter = ref 0 in
  open_graph "";
  draw_start_screen ();
  wait_for_space_or_a ai;
  let state = State.initialize () in
  draw_game_screen state;
  (* Main game loop that runs until game over. *)  
  while not (game_over state) do
    sleepf 0.05;
    (* Gets current difficulty level to change how fast the game goes. *)
    let diff = 11 - (get_level state) in
    (* Checks if there is an input from the player. *)
    let () = if !ai then play_game_ai state else play_game_user state in
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
  let t = Unix.localtime (Unix.time ()) in
  write_highscore_file ai (t.tm_mon+1) t.tm_mday (t.tm_year+1900) (t.tm_hour) 
    (t.tm_min) (get_score state);
  wait_for_space ();
  main ()

let () = main ()