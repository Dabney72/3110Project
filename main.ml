open Graphics
open Display
open Tetromino
open State
open Unix  
open Str

(**[read_lines name] reads lines froma given text file to a int int list for
   the purposes of this game only*)

let read_lines name =
  let file = open_in name in
  let try_read () =
    try Some (input_line file) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> begin
        if Str.string_match (Str.regexp "[0-9]+") s 0 = true then 
          loop (List.map int_of_string (Str.split (Str.regexp "[^0-9]+") s) :: acc)
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
      |[_;_;_;_;_;a1],[_;_;_;_;_;a2]-> if a1 > a2 then -1 else 1
      | _,_ -> failwith "Error in comparison"
    end
  else 
    failwith "Lengths are not the same"


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
  if Sys.file_exists "highscore.txt" = false 
  then 
    begin
      let t = Unix.localtime (Unix.time ()) in
      let oc = open_out "highscore.txt" in
      Printf.fprintf oc "%s" "Month Day Year Hour(24-hr) Minute     Score\n";
      Printf.fprintf oc "%d %5d %5d %5d %1od %11d \n"  (t.tm_mon+1) t.tm_mday 
        (t.tm_year+1900) (t.tm_hour) (t.tm_min) (get_score state);
      close_out oc; 
      wait_for_space ();
      main ()
    end
  else 
    begin
      let t = Unix.localtime (Unix.time ()) in
      let nc = read_lines "highscore.txt" in 
      let nl = List.sort mycompare ([(t.tm_mon+1);t.tm_mday;(t.tm_year+1900);
                                     (t.tm_hour);(t.tm_min);(get_score state)]::nc)
      in
      let oc = open_out "highscore.txt" in
      Printf.fprintf oc "%s" "Month Day Year Hour(24-hr) Minute     Score \n";
      let rec make_file list= 
        match list with 
        | [] -> close_out oc;
        | [a;b;c;d;e;f]::t -> begin
            Printf.fprintf oc "%d %5d %5d %5d %10d %11d \n" a b c d e f; 
            make_file t;
          end
        | _ -> failwith "Error"
      in make_file nl;
      wait_for_space ();
      main ()
    end
let () = main ()