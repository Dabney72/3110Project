open Graphics
open Display
open State
open Tetromino
open Unix 

let start () =
  open_graph "";
  draw_start_screen ();
  (*Display.draw_grid ();*)
  ignore (read_key () : char);
  clear_graph ();
  let init = State.initialize () in
  spawn_tetromino (Tetromino.init_tetromino J_block) init;
  drop init;
  draw_game_screen init;  
  (* Used Unix.sleep for the interval. I think it is pretty straightfoward that
     Unix.sleep takes in an int, which is represented in seconds. times has to
     be inputted manually so I guess put the largest number Ocaml can take if we 
     are to make this work *)
  let rec print_hello times =
    Unix.sleep 3;
    if times <> 0 then (print_endline "Hello World!"; print_hello (times-1)) 
    else print_endline "Hello World"; 
  in print_hello 10;
  while true do
    let st = wait_next_event [ Mouse_motion; Button_down; Key_pressed ] in
    synchronize ();
    if st.keypressed then close_graph ();
  done

let () = start ()