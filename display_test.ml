open Display
open State

let test_start () = 
  draw_start_screen ()

let test_base_game_state () =
  let state = initialize () in
  draw_game_screen state