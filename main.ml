open Graphics
open Display

let start () =
  open_graph "";
  Display.draw_start_screen ();
  (*Display.draw_grid ();*)
  ignore (read_key () : char);
  clear_graph ();
  Display.draw_grid (Array.make_matrix 10 20 0);
  while true do
    let st = wait_next_event [ Mouse_motion; Button_down; Key_pressed ] in
    synchronize ();
    if st.keypressed then close_graph ();
  done

let () = start ()