open Graphics
open Display
open State

let start () =
  open_graph "";
  Display.draw_start_screen ();
  (*Display.draw_grid ();*)
  ignore (read_key () : char);
  clear_graph ();
  (* This what we want to use but have not implemented Tetromino.generate_list
     so it fails with Failure("Unimplemented") right now
     let init = State.initialize () in
     Display.draw_grid (State.get_grid init); 
     Right now we hard code the array to make the list. 
  *) 
  Display.draw_grid (Array.make_matrix 10 20 0);
  while true do
    let st = wait_next_event [ Mouse_motion; Button_down; Key_pressed ] in
    synchronize ();
    if st.keypressed then close_graph ();
  done

let () = start ()