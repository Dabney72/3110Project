open OUnit2
open State
open Tetromino
open Printers


(** [to_intgrid arr] turns the 2D Tetromino option [arr] into a 2D int grid such
    that None is 0 and Some Tetromino is 1. *)
let to_intgrid arr =
  let convert = function
    | None -> 0
    | Some Shadow -> 2
    | Some _ -> 1 in 
  let convertrow = Array.map convert in 
  Array.map convertrow arr

(** [spawn_tetromino_test name st tetromino grid] is an OUnit test case named 
    after [tetromino] for [spawn_tetromino st tetromino] asserting that 
    the output is [grid]. *)
let spawn_tetromino_test name st tetromino grid =
  name >:: fun ctxt ->  
    assert_equal (grid ()) (spawn_tetromino tetromino st; st |> get_grid |> to_intgrid) 
      ~printer: pp_int_matrix

(** [grid_test name st grid] is an OUnit test case named [name] for 
    [st] asserting that its grid attribute is [grid]. *)
let grid_test name st grid =
  name >:: fun ctxt ->  
    assert_equal grid (st |> get_grid |> to_intgrid) ~printer: pp_int_matrix

(** [game_over_test name st expected] is an OUnit test case named [name] for
    [st], asserting that [game_over st] is [expected]. *)
let game_over_test name st expected =
  name >:: fun _ ->
    assert_equal expected (game_over st) ~printer: string_of_bool

(** [copy_grid_test name st] is an OUnit test case named [name], ensuring that
    the grid in [st] is not mutated when a copy of it is mutated. *)
let copy_grid_test name st =
  let grid = copy_grid_int st in
  let grid_mut = copy_grid_int st in
  Array.iter (fun arr -> arr.(1) <- 3) grid_mut;
  name >:: fun _ -> assert_equal grid (get_grid st |> to_intgrid)

(********************************************************************
   Initilization and Spawn Testing
 ********************************************************************)

(* Matrix creation *)
let initial = State.initialize ~auto_spawn: false
let initial_autospawn = State.initialize
let tenbyseventeen = Array.make_matrix 17 10 0
let prepend3 top3 = Array.append top3 tenbyseventeen 
let append3 bot3 = Array.append tenbyseventeen bot3

(* Top three rows of each tetromino type. *)
let i_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let l_top3 = [|
  [|0;0;0;0;0;1;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let j_top3 = [|
  [|0;0;0;1;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let o_top3 = [|
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let s_top3 = [|
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let t_top3 = [|
  [|0;0;0;0;1;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let z_top3 = [|
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

(* 10x20 grid corresponding to the spawn of each *)
let i_grid () = prepend3 i_top3
let l_grid () = prepend3 l_top3
let j_grid () = prepend3 j_top3 
let o_grid () = prepend3 o_top3 
let s_grid () = prepend3 s_top3 
let t_grid () = prepend3 t_top3 
let z_grid () = prepend3 z_top3 

let spawn_tetromino_tests = [
  spawn_tetromino_test "spawn iblock" (initial ()) I_block i_grid;
  spawn_tetromino_test "spawn lblock" (initial ()) L_block l_grid;
  spawn_tetromino_test "spawn jblock" (initial ()) J_block j_grid;
  spawn_tetromino_test "spawn oblock" (initial ()) O_block o_grid;
  spawn_tetromino_test "spawn sblock" (initial ()) S_block s_grid;
  spawn_tetromino_test "spawn tblock" (initial ()) T_block t_grid;
  spawn_tetromino_test "spawn zblock" (initial ()) Z_block z_grid;
]

(********************************************************************
   Movement Testing
 ********************************************************************)

(* Helper functions *)
let spawn block st = st |> spawn_tetromino block; st
let block_left block st =  st |> spawn block |> move_left; st
let block_right block st =  st |> spawn block |> move_right; st
let block_down block st =  st |> spawn block |> fall ~auto_respawn: false; st
let spawn_and_drop block st = st |> spawn block |> drop ~auto_respawn: false; st

let drop_block st = st |> drop ~auto_respawn: false; st
let auto_drop_n n st = for i = 1 to n do st |> drop done; st
let left n st = for i = 1 to n do st |> move_left done; st
let right n st = for i = 1 to n do st |> move_right done; st
let down n st = for i = 1 to n do st |> fall ~auto_respawn: false done; st
let rotate n st = for i = 1 to n do st |> State.rotate_cw done; st

(** [spawn_move_drop tetr n_rot n_l n_r st] spawns [tetr] in [st], rotates it
    [n_rot] times, then moves it [n_l] blocks to the left, then [n_r] blocks to
    the right, then drops it. *)
let spawn_move_drop tetr n_rot n_l n_r st = 
  st 
  |> spawn tetr 
  |> rotate n_rot 
  |> left n_l 
  |> right n_r 
  |> drop ~auto_respawn: false;
  st

let spawn_move_drop_n tetr n_rot n_l n_r n st = 
  let rec loop st i =
    if i > n then st
    else loop (spawn_move_drop tetr n_rot n_l n_r st) (i + 1)
  in loop st 1

(* Spawn block, move it left then return the state. *)
let i_left = initial () |> block_left I_block
let l_left = initial () |> block_left L_block
let j_left = initial () |> block_left J_block
let o_left = initial () |> block_left O_block
let s_left = initial () |> block_left S_block
let t_left = initial () |> block_left T_block
let z_left = initial () |> block_left Z_block

(* Spawn block, move it right then return the state.*)
let i_right = initial () |> block_right I_block
let l_right = initial () |> block_right L_block
let j_right = initial () |> block_right J_block
let o_right = initial () |> block_right O_block
let s_right = initial () |> block_right S_block
let t_right = initial () |> block_right T_block
let z_right = initial () |> block_right Z_block

(* Spawn block, fall down one row then return the state.*)
let i_down = initial () |> block_down I_block
let l_down = initial () |> block_down L_block
let j_down = initial () |> block_down J_block
let o_down = initial () |> block_down O_block
let s_down = initial () |> block_down S_block
let t_down = initial () |> block_down T_block
let z_down = initial () |> block_down Z_block

(* Spawn block, drop to bottom then return the state.*)
let i_drop = initial () |> spawn_and_drop I_block
let l_drop = initial () |> spawn_and_drop L_block
let j_drop = initial () |> spawn_and_drop J_block
let o_drop = initial () |> spawn_and_drop O_block
let s_drop = initial () |> spawn_and_drop S_block
let t_drop = initial () |> spawn_and_drop T_block
let z_drop = initial () |> spawn_and_drop Z_block

(* Spawn block, move it all the way to the left then return the state.*)
let i_lmost = initial () |> spawn I_block |> left 10
let l_lmost = initial () |> spawn L_block |> left 10
let j_lmost = initial () |> spawn J_block |> left 10
let o_lmost = initial () |> spawn O_block |> left 10
let s_lmost = initial () |> spawn S_block |> left 10
let t_lmost = initial () |> spawn T_block |> left 10
let z_lmost = initial () |> spawn Z_block |> left 10

(* Spawn block, move it all the way to the left then return the state.*)
let i_rmost = initial () |> spawn I_block |> right 10
let l_rmost = initial () |> spawn L_block |> right 10
let j_rmost = initial () |> spawn J_block |> right 10
let o_rmost = initial () |> spawn O_block |> right 10
let s_rmost = initial () |> spawn S_block |> right 10
let t_rmost = initial () |> spawn T_block |> right 10
let z_rmost = initial () |> spawn Z_block |> right 10

(* Spawn block, move it all the way to the down using fall then return the state.*)
let i_downmost = initial () |> spawn I_block |> down 20
let l_downmost = initial () |> spawn L_block |> down 20
let j_downmost = initial () |> spawn J_block |> down 20
let o_downmost = initial () |> spawn O_block |> down 20
let s_downmost = initial () |> spawn S_block |> down 20
let t_downmost = initial () |> spawn T_block |> down 20
let z_downmost = initial () |> spawn Z_block |> down 20

(* Fill the first 4 rows entirely, except for last column, using different 
   rotations and translations of each tetromino. *)
let block_4x9 = initial ()
                |> spawn_move_drop T_block 0 0 1
                |> spawn_move_drop S_block 0 1 0
                |> spawn_move_drop O_block 0 4 0
                |> spawn_move_drop J_block 3 0 4
                |> spawn_move_drop T_block 2 2 0
                |> spawn_move_drop Z_block 0 0 0
                |> spawn_move_drop O_block 0 0 2
                |> spawn_move_drop L_block 2 3 0
                |> spawn_move_drop I_block 0 0 2

let i_rotation_left = initial ()
                      |> spawn I_block
                      |> rotate 1
                      |> left 10
                      |> rotate 1
                      |> drop_block

let i_rotation_right = initial ()
                       |> spawn I_block
                       |> rotate 1
                       |> right 10
                       |> rotate 1
                       |> drop_block

(* Drop the next 20 pieces in the game, which will overflow the board. *)
let random_overflow = initial_autospawn () |> auto_drop_n 20

let i_top = initial () |> spawn I_block

let overflow blk = i_top |> spawn blk

let tenbyfourteen = Array.make_matrix 14 10 0

let combine top3 bottom = bottom |> Array.append tenbyfourteen |> Array.append top3

(* Top three rows of spawning each tetromino block then moving it left. *)
let i_left_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;1;1;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let i_left_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;2;2;2;2;0;0;0;0|]
|]

let l_left_top3 = [|
  [|0;0;0;0;1;0;0;0;0;0|]; 
  [|0;0;1;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let l_left_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;2;0;0;0;0;0|]; 
  [|0;0;2;2;2;0;0;0;0;0|]
|]

let j_left_top3 = [|
  [|0;0;1;0;0;0;0;0;0;0|]; 
  [|0;0;1;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let j_left_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;2;0;0;0;0;0;0;0|]; 
  [|0;0;2;2;2;0;0;0;0;0|]
|]

let o_left_top3 = [|
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let o_left_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;2;2;0;0;0;0;0|]; 
  [|0;0;0;2;2;0;0;0;0;0|]
|]

let s_left_top3 = [|
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;1;1;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let s_left_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;2;2;0;0;0;0;0|]; 
  [|0;0;2;2;0;0;0;0;0;0|]
|]

let t_left_top3 = [|
  [|0;0;0;1;0;0;0;0;0;0|]; 
  [|0;0;1;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let t_left_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;2;0;0;0;0;0;0|]; 
  [|0;0;2;2;2;0;0;0;0;0|]
|]

let z_left_top3 = [|
  [|0;0;1;1;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let z_left_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;2;2;0;0;0;0;0;0|]; 
  [|0;0;0;2;2;0;0;0;0;0|]
|]

(* Top three rows of spawning each tetromino block then moving it to leftmost. *)
let i_lmost_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|1;1;1;1;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let i_lmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|2;2;2;2;0;0;0;0;0;0|]
|]

let l_lmost_top3 = [|
  [|0;0;1;0;0;0;0;0;0;0|]; 
  [|1;1;1;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let l_lmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;2;0;0;0;0;0;0;0|]; 
  [|2;2;2;0;0;0;0;0;0;0|]
|]

let j_lmost_top3 = [|
  [|1;0;0;0;0;0;0;0;0;0|]; 
  [|1;1;1;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let j_lmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|2;0;0;0;0;0;0;0;0;0|]; 
  [|2;2;2;0;0;0;0;0;0;0|]
|]

let o_lmost_top3 = [|
  [|1;1;0;0;0;0;0;0;0;0|]; 
  [|1;1;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let o_lmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|2;2;0;0;0;0;0;0;0;0|]; 
  [|2;2;0;0;0;0;0;0;0;0|]
|]

let s_lmost_top3 = [|
  [|0;1;1;0;0;0;0;0;0;0|]; 
  [|1;1;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let s_lmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;2;2;0;0;0;0;0;0;0|]; 
  [|2;2;0;0;0;0;0;0;0;0|]
|]

let t_lmost_top3 = [|
  [|0;1;0;0;0;0;0;0;0;0|]; 
  [|1;1;1;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let t_lmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;2;0;0;0;0;0;0;0;0|]; 
  [|2;2;2;0;0;0;0;0;0;0|]
|]

let z_lmost_top3 = [|
  [|1;1;0;0;0;0;0;0;0;0|]; 
  [|0;1;1;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let z_lmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|2;2;0;0;0;0;0;0;0;0|]; 
  [|0;2;2;0;0;0;0;0;0;0|]
|]

(* Top three rows of spawning each tetromino block then moving it right. *)
let i_right_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;1;1;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let i_right_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;2;2;2;2;0;0|]
|]

let l_right_top3 = [|
  [|0;0;0;0;0;0;1;0;0;0|]; 
  [|0;0;0;0;1;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let l_right_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;2;0;0;0|]; 
  [|0;0;0;0;2;2;2;0;0;0|]
|]

let j_right_top3 = [|
  [|0;0;0;0;1;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let j_right_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;2;0;0;0;0;0|]; 
  [|0;0;0;0;2;2;2;0;0;0|]
|]

let o_right_top3 = [|
  [|0;0;0;0;0;1;1;0;0;0|]; 
  [|0;0;0;0;0;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let o_right_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;2;2;0;0;0|]; 
  [|0;0;0;0;0;2;2;0;0;0|]
|]

let s_right_top3 = [|
  [|0;0;0;0;0;1;1;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let s_right_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;2;2;0;0;0|]; 
  [|0;0;0;0;2;2;0;0;0;0|]
|]

let t_right_top3 = [|
  [|0;0;0;0;0;1;0;0;0;0|]; 
  [|0;0;0;0;1;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let t_right_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;2;0;0;0;0|]; 
  [|0;0;0;0;2;2;2;0;0;0|]
|]

let z_right_top3 = [|
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let z_right_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;2;2;0;0;0;0|]; 
  [|0;0;0;0;0;2;2;0;0;0|]
|]

(* Top three rows of spawning each tetromino block then moving it to rightmost. *)
let i_rmost_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;1;1;1;1|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let i_rmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;2;2;2;2|]
|]

let l_rmost_top3 = [|
  [|0;0;0;0;0;0;0;0;0;1|]; 
  [|0;0;0;0;0;0;0;1;1;1|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let l_rmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;2|]; 
  [|0;0;0;0;0;0;0;2;2;2|]
|]

let j_rmost_top3 = [|
  [|0;0;0;0;0;0;0;1;0;0|]; 
  [|0;0;0;0;0;0;0;1;1;1|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let j_rmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;2;0;0|]; 
  [|0;0;0;0;0;0;0;2;2;2|]
|]

let o_rmost_top3 = [|
  [|0;0;0;0;0;0;0;0;1;1|]; 
  [|0;0;0;0;0;0;0;0;1;1|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let o_rmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;2;2|]; 
  [|0;0;0;0;0;0;0;0;2;2|]
|]

let s_rmost_top3 = [|
  [|0;0;0;0;0;0;0;0;1;1|]; 
  [|0;0;0;0;0;0;0;1;1;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let s_rmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;2;2|]; 
  [|0;0;0;0;0;0;0;2;2;0|]
|]

let t_rmost_top3 = [|
  [|0;0;0;0;0;0;0;0;1;0|]; 
  [|0;0;0;0;0;0;0;1;1;1|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let t_rmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;2;0|]; 
  [|0;0;0;0;0;0;0;2;2;2|]
|]

let z_rmost_top3 = [|
  [|0;0;0;0;0;0;0;1;1;0|]; 
  [|0;0;0;0;0;0;0;0;1;1|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let z_rmost_bot = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;2;2;0|]; 
  [|0;0;0;0;0;0;0;0;2;2|]
|]

(* Top three rows of spawning each tetromino block then moving it down a row. *)
let i_down_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;1;0;0;0|]
|]

let l_down_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;1;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]
|]
let j_down_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;1;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]
|]
let o_down_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]
|]
let s_down_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;1;1;0;0;0;0;0|]
|]
let t_down_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;1;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]
|]
let z_down_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]
|]

(* Top three rows of spawning each tetromino block then moving it to leftmost. *)
let i_dmost_bot3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;1;0;0;0|]
|]
let l_dmost_bot3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;1;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]
|]
let j_dmost_bot3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;1;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]
|]
let o_dmost_bot3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]
|]
let s_dmost_bot3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;1;1;0;0;0;0;0|]
|]
let t_dmost_bot3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;1;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]
|]
let z_dmost_bot3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]
|]

let almost_filled = [|
  [|1;1;1;1;1;1;1;1;1;0|];
  [|1;1;1;1;1;1;1;1;1;0|];
  [|1;1;1;1;1;1;1;1;1;0|];
  [|1;1;1;1;1;1;1;1;1;0|]
|]

let i_rotated_lmost = [|
  [|1;0;0;0;0;0;0;0;0;0|];
  [|1;0;0;0;0;0;0;0;0;0|];
  [|1;0;0;0;0;0;0;0;0;0|];
  [|1;0;0;0;0;0;0;0;0;0|]
|]

let i_rotated_rmost = [|
  [|0;0;0;0;0;0;0;0;0;1|];
  [|0;0;0;0;0;0;0;0;0;1|];
  [|0;0;0;0;0;0;0;0;0;1|];
  [|0;0;0;0;0;0;0;0;0;1|]
|]

let i_left_grid = combine i_left_top3 i_left_bot
let l_left_grid = combine l_left_top3 l_left_bot
let j_left_grid = combine j_left_top3 j_left_bot
let o_left_grid = combine o_left_top3 o_left_bot
let s_left_grid = combine s_left_top3 s_left_bot
let t_left_grid = combine t_left_top3 t_left_bot
let z_left_grid = combine z_left_top3 z_left_bot

let i_lmost_grid = combine i_lmost_top3 i_lmost_bot
let l_lmost_grid = combine l_lmost_top3 l_lmost_bot
let j_lmost_grid = combine j_lmost_top3 j_lmost_bot
let o_lmost_grid = combine o_lmost_top3 o_lmost_bot
let s_lmost_grid = combine s_lmost_top3 s_lmost_bot
let t_lmost_grid = combine t_lmost_top3 t_lmost_bot
let z_lmost_grid = combine z_lmost_top3 z_lmost_bot


let filled_4 = Array.append (Array.make_matrix 16 10 0) almost_filled
let i_rot_left = Array.append (Array.make_matrix 16 10 0) i_rotated_lmost
let i_rot_right = Array.append (Array.make_matrix 16 10 0) i_rotated_rmost

let move_left_tests = [
  grid_test "move spawned i block left" i_left i_left_grid;
  grid_test "move spawned l block left" l_left l_left_grid;
  grid_test "move spawned j block left" j_left j_left_grid;
  grid_test "move spawned o block left" o_left o_left_grid;
  grid_test "move spawned s block left" s_left s_left_grid;
  grid_test "move spawned t block left" t_left t_left_grid;
  grid_test "move spawned z block left" z_left z_left_grid;
  grid_test "move spawned i block lmost" i_lmost i_lmost_grid;
  grid_test "move spawned l block lmost" l_lmost l_lmost_grid;
  grid_test "move spawned j block lmost" j_lmost j_lmost_grid;
  grid_test "move spawned o block lmost" o_lmost o_lmost_grid;
  grid_test "move spawned s block lmost" s_lmost s_lmost_grid;
  grid_test "move spawned t block lmost" t_lmost t_lmost_grid;
  grid_test "move spawned z block lmost" z_lmost z_lmost_grid;
  grid_test "rotate i block at left edge" i_rotation_left i_rot_left;
  grid_test "rotate i block at right edge" i_rotation_right i_rot_right;
]

let i_right_grid = combine i_right_top3 i_right_bot
let l_right_grid = combine l_right_top3 l_right_bot
let j_right_grid = combine j_right_top3 j_right_bot
let o_right_grid = combine o_right_top3 o_right_bot
let s_right_grid = combine s_right_top3 s_right_bot
let t_right_grid = combine t_right_top3 t_right_bot
let z_right_grid = combine z_right_top3 z_right_bot

let i_rmost_grid = combine i_rmost_top3 i_rmost_bot
let l_rmost_grid = combine l_rmost_top3 l_rmost_bot
let j_rmost_grid = combine j_rmost_top3 j_rmost_bot
let o_rmost_grid = combine o_rmost_top3 o_rmost_bot
let s_rmost_grid = combine s_rmost_top3 s_rmost_bot
let t_rmost_grid = combine t_rmost_top3 t_rmost_bot
let z_rmost_grid = combine z_rmost_top3 z_rmost_bot

let move_right_tests = [
  grid_test "move spawned i block right" i_right i_right_grid;
  grid_test "move spawned l block right" l_right l_right_grid;
  grid_test "move spawned j block right" j_right j_right_grid;
  grid_test "move spawned o block right" o_right o_right_grid;
  grid_test "move spawned s block right" s_right s_right_grid;
  grid_test "move spawned t block right" t_right t_right_grid;
  grid_test "move spawned z block right" z_right z_right_grid;
  grid_test "move spawned i block rmost" i_rmost i_rmost_grid;
  grid_test "move spawned l block rmost" l_rmost l_rmost_grid;
  grid_test "move spawned j block rmost" j_rmost j_rmost_grid;
  grid_test "move spawned o block rmost" o_rmost o_rmost_grid;
  grid_test "move spawned s block rmost" s_rmost s_rmost_grid;
  grid_test "move spawned t block rmost" t_rmost t_rmost_grid;
  grid_test "move spawned z block rmost" z_rmost z_rmost_grid;
]

let row_to_shadow = Array.map (fun x -> if x = 1 then 2 else 0) 

let to_shadow grid = Array.map row_to_shadow grid

let combine_down grid1 grid2 =  grid2 |> to_shadow |> combine grid1

let i_down_grid = combine_down i_down_top3 i_down_top3
let l_down_grid = combine_down l_down_top3 l_down_top3
let j_down_grid = combine_down j_down_top3 j_down_top3
let o_down_grid = combine_down o_down_top3 o_down_top3
let s_down_grid = combine_down s_down_top3 s_down_top3
let t_down_grid = combine_down t_down_top3 t_down_top3
let z_down_grid = combine_down z_down_top3 z_down_top3

let fall_tests = [
  grid_test "move spawned i block down" i_down i_down_grid;
  grid_test "move spawned l block down" l_down l_down_grid;
  grid_test "move spawned j block down" j_down j_down_grid;
  grid_test "move spawned o block down" o_down o_down_grid;
  grid_test "move spawned s block down" s_down s_down_grid;
  grid_test "move spawned t block down" t_down t_down_grid;
  grid_test "move spawned z block down" z_down z_down_grid;
  grid_test "move spawned i block dmost" i_downmost (append3 i_dmost_bot3);
  grid_test "move spawned l block dmost" l_downmost (append3 l_dmost_bot3);
  grid_test "move spawned j block dmost" j_downmost (append3 j_dmost_bot3);
  grid_test "move spawned o block dmost" o_downmost (append3 o_dmost_bot3);
  grid_test "move spawned s block dmost" s_downmost (append3 s_dmost_bot3);
  grid_test "move spawned t block dmost" t_downmost (append3 t_dmost_bot3);
  grid_test "move spawned z block dmost" z_downmost (append3 z_dmost_bot3);
]

let drop_tests = [
  grid_test "drop spawned i block down" i_drop (append3 i_down_top3);
  grid_test "drop spawned l block down" l_drop (append3 l_down_top3);
  grid_test "drop spawned j block down" j_drop (append3 j_down_top3);
  grid_test "drop spawned o block down" o_drop (append3 o_down_top3);
  grid_test "drop spawned s block down" s_drop (append3 s_down_top3);
  grid_test "drop spawned t block down" t_drop (append3 t_down_top3);
  grid_test "drop spawned z block down" z_drop (append3 z_down_top3);
  grid_test "fill first 4 rows" block_4x9 filled_4;
]

let movement_tests = List.flatten [
    move_left_tests;
    move_right_tests;
    fall_tests;
    drop_tests;
  ]

let game_over_tests = [
  game_over_test "Overflow causes game over" random_overflow true;
  game_over_test "Grid of height 4" block_4x9 false;
  game_over_test "i overflow" (overflow I_block) true;
  game_over_test "l overflow" (overflow L_block) true;
  game_over_test "j overflow" (overflow J_block) true;
  game_over_test "o overflow" (overflow O_block) true;
  game_over_test "s overflow" (overflow S_block) true;
  game_over_test "t overflow" (overflow T_block) true;
  game_over_test "z overflow" (overflow Z_block) true;
]

let other_tests = [
  copy_grid_test "empty copy" (initial ());
  copy_grid_test "4x9 copy" block_4x9;
]

(********************************************************************
   Score Testing
 ********************************************************************)

let make_grid height = 
  let rest = Array.make_matrix (20 - height) 10 None in
  let grid = Array.make_matrix height 10 None in 
  let change_last_col arr i x = 
    if i = 9 then arr.(i)  <- None else arr.(i) <- Some I_block in
  let fix_row arr = 
    Array.iteri (change_last_col arr) arr in
  Array.iter fix_row grid;
  Array.append rest grid

let sim_fall st = fall st; st

let simulate_lines_cleared ln combo = 
  let st = ref (initial () |> update_grid (make_grid ln)) in
  for i = 1 to combo do 
    st := !st
          |> spawn I_block
          |> sim_fall
          |> rotate 1
          |> right 5
          |> drop_block
  done;
  get_score !st


let score_test name output ln combo =
  name >:: (fun _ -> assert_equal output (simulate_lines_cleared ln combo)
               ~printer: string_of_int)

let score_tests = [
  score_test "one line cleared" 40 1 1;
  score_test "two consecutive lines cleared" 100 2 1;
  score_test "three consecutive lines cleared" 300 3 1;
  score_test "four consecutive lines cleared" 1200 4 1;
  score_test "combo of two four line clears back to back" 3600 8 2;
  score_test "combo of three four line clears back to back" 7200 12 3;
]

(********************************************************************
   Level and Lines Cleared Testing
 ********************************************************************)

let level_test name output st ln =
  name >:: (fun _ -> assert_equal output 
               (increment_lines_cleared st ln; get_level st))

let lines_cleared_test name output st ln =
  name >:: (fun _ -> assert_equal output 
               (increment_lines_cleared st ln; get_lines_cleared st))

let make_level_10_game = 
  let x = (initialize ()) in
  for i = 0 to 9 do
    increment_lines_cleared x 10;
  done;
  x

let level_and_lines_tests = [
  lines_cleared_test "adding 0 lines cleared to base game state" 
    0 (initialize ()) 0;
  lines_cleared_test "adding 1 line cleared to base game state"
    1 (initialize ()) 1;
  lines_cleared_test "adding 4 lines cleared to base game state"
    4 (initialize ()) 4;
  level_test "adding 1 line to base game state doesn't increase level"
    1 (initialize ()) 1;
  level_test "adding 10 lines to base game state increases level from 1 to 2" 
    2 (initialize ()) 10;
  level_test "adding 10 lines to base game 9 times gets it to level 10"
    10 make_level_10_game 0;
  level_test "adding 10 lines to game at level 10 doesn't increase level"
    10 make_level_10_game 10;
]

(********************************************************************
   Hold Testing
 ********************************************************************)

let initial_x = (initialize ())
let initial_y = (initialize ())

let hold_tests = [
  "ability to hold is true at initialization" >:: 
  (fun _ -> assert_equal true (use_hold (initial_x)) ~printer:string_of_bool);
  "using hold function on a new block sets ability to use hold to false" >:: 
  (fun _ -> assert_equal false (hold initial_x ; use_hold initial_x) ~printer:string_of_bool);
  "using hold function over and over without new block is false" >::
  (fun _ -> assert_equal false (hold initial_x ;hold initial_x;hold initial_x;hold initial_x;
                                use_hold initial_x) ~printer:string_of_bool);
  "drop block makes hold function usable again" >::
  (fun _ -> assert_equal true (hold initial_x ;hold initial_x;hold initial_x;hold initial_x;
                               drop initial_x; use_hold initial_x) ~printer:string_of_bool);   
  "drop block and then using hold makes hold function unusable" >::
  (fun _ -> assert_equal false (hold initial_x ;hold initial_x;hold initial_x;hold initial_x;
                                drop initial_x; hold initial_x; use_hold initial_x) 
      ~printer:string_of_bool);                           
]

(********************************************************************
   End Helper Suites
 ********************************************************************)

let suite =
  "test suite"  >::: List.flatten [
    spawn_tetromino_tests;
    movement_tests;
    game_over_tests;
    score_tests;
    level_and_lines_tests;
    hold_tests;
    other_tests;
  ]

let _ = print_newline (); print_endline "Running State Tests..."
let _ = run_test_tt_main suite