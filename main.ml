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
  let state = initialize () in
  draw_game_screen state;  
  (* Used Unix.sleep for the interval. I think it is pretty straightfoward that
     Unix.sleep takes in an int, which is represented in seconds. times has to
     be inputted manually so I guess put the largest number Ocaml can take if we 
     are to make this work *)
  let rec game () =
    Unix.sleepf 0.75;
    spawn_next state; move_left state; drop state; draw_game_screen state; game ()
  in game ();
  while true do
    let st = wait_next_event [ Mouse_motion; Button_down; Key_pressed ] in
    synchronize ();
    if st.keypressed then close_graph ();
  done

let () = start ()