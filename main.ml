open Graphics
open Display
open State
open Tetromino

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
  while true do
    let st = wait_next_event [ Mouse_motion; Button_down; Key_pressed ] in
    synchronize ();
    if st.keypressed then close_graph ();
  done

let () = start ()