open OUnit2
open State
open Tetromino

(** [to_intgrid arr] turns the 2D Tetromino option [arr] into a 2D int grid such
    that None is 0 and Some Tetromino is 1. *)
let to_intgrid arr =
  let convert = function
    | None -> 0
    | Some _ -> 1 in 
  let convertrow = Array.map convert in 
  Array.map convertrow arr

(** [pp_array arr] pretty prints an int array [arr]. *)
let pp_array arr = 
  let build_string acc x = 
    acc ^ "; " ^ string_of_int x in
  let s = Array.fold_left build_string "" arr in 
  "[|" ^ String.sub s 2 (String.length s - 2) ^ "|]"

(** [pp_matrix m] pretty prints an int matrix [m]. *)
let pp_matrix (m : int array array) = 
  let aux acc row = 
    acc ^ "; \n" ^ pp_array row  in
  let s = Array.fold_left aux "" m in 
  "[|" ^ String.sub s 2 (String.length s - 2) ^ "\n|]"

(** [spawn_tetromino_test name st tetromino grid] is an OUnit test case named 
    after [tetromino] for [spawn_tetromino st tetromino] asserting that 
    the output is [grid]. *)
let spawn_tetromino_test name st tetromino grid =
  name >:: fun ctxt ->  
    assert_equal (grid ()) (spawn_tetromino tetromino st; st |> get_grid |> to_intgrid) 
      ~printer: pp_matrix

(** [grid_test name st grid] is an OUnit test case named [name] for 
    [st] asserting that its grid attribute is [grid]. *)
let grid_test name st grid =
  name >:: fun ctxt ->  
    assert_equal grid (st |> get_grid |> to_intgrid) ~printer: pp_matrix

(********************************************************************
   Initilization and Spawn Testing
 ********************************************************************)

(* Matrix creation *)
let initial = State.initialize ~auto_spawn: false
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
let left n st = for i = 1 to n do st |> move_left done; st
let right n st = for i = 1 to n do st |> move_right done; st
let down n st = for i = 1 to n do st |> fall ~auto_respawn: false done; st
let rotate n st = for i = 1 to n do st |> State.rotate_cw done; st

(* Spawn block, move it left then return the state.*)
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
let i_leftmost = initial () |> spawn I_block |> left 10
let l_leftmost = initial () |> spawn L_block |> left 10
let j_leftmost = initial () |> spawn J_block |> left 10
let o_leftmost = initial () |> spawn O_block |> left 10
let s_leftmost = initial () |> spawn S_block |> left 10
let t_leftmost = initial () |> spawn T_block |> left 10
let z_leftmost = initial () |> spawn Z_block |> left 10

(* Spawn block, move it all the way to the left then return the state.*)
let i_rightmost = initial () |> spawn I_block |> right 10
let l_rightmost = initial () |> spawn L_block |> right 10
let j_rightmost = initial () |> spawn J_block |> right 10
let o_rightmost = initial () |> spawn O_block |> right 10
let s_rightmost = initial () |> spawn S_block |> right 10
let t_rightmost = initial () |> spawn T_block |> right 10
let z_rightmost = initial () |> spawn Z_block |> right 10

(* Spawn block, move it all the way to the down using fall then return the state.*)
let i_downmost = initial () |> spawn I_block |> down 20
let l_downmost = initial () |> spawn L_block |> down 20
let j_downmost = initial () |> spawn J_block |> down 20
let o_downmost = initial () |> spawn O_block |> down 20
let s_downmost = initial () |> spawn S_block |> down 20
let t_downmost = initial () |> spawn T_block |> down 20
let z_downmost = initial () |> spawn Z_block |> down 20

(* Fill the first 4 rows entirely, using different rotations and translations
   of each tetromino. *)
let block_4x10 = initial ()
                 |> spawn T_block |> right 1 |> drop_block
                 |> spawn S_block |> left 1 |> drop_block
                 |> spawn O_block |> left 4 |> drop_block
                 |> spawn J_block |> rotate 1 |> right 4 |> drop_block
                 |> spawn T_block |> rotate 2 |> left 2 |> drop_block
                 |> spawn Z_block |> drop_block
                 |> spawn O_block |> right 2 |> drop_block
                 |> spawn L_block |> rotate 2 |> left 3 |> drop_block
                 |> spawn I_block |> right 2 |> drop_block

(* Top three rows of spawning each tetrimno block then moving it left. *)
let i_left_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;1;1;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let l_left_top3 = [|
  [|0;0;0;0;1;0;0;0;0;0|]; 
  [|0;0;1;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let j_left_top3 = [|
  [|0;0;1;0;0;0;0;0;0;0|]; 
  [|0;0;1;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let o_left_top3 = [|
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let s_left_top3 = [|
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;1;1;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let t_left_top3 = [|
  [|0;0;0;1;0;0;0;0;0;0|]; 
  [|0;0;1;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let z_left_top3 = [|
  [|0;0;1;1;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

(** Top three rows of spawning each tetrimino block then moving it to leftmost.*)
let i_lmost_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|1;1;1;1;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let l_lmost_top3 = [|
  [|0;0;1;0;0;0;0;0;0;0|]; 
  [|1;1;1;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let j_lmost_top3 = [|
  [|1;0;0;0;0;0;0;0;0;0|]; 
  [|1;1;1;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let o_lmost_top3 = [|
  [|1;1;0;0;0;0;0;0;0;0|]; 
  [|1;1;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let s_lmost_top3 = [|
  [|0;1;1;0;0;0;0;0;0;0|]; 
  [|1;1;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let t_lmost_top3 = [|
  [|0;1;0;0;0;0;0;0;0;0|]; 
  [|1;1;1;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let z_lmost_top3 = [|
  [|1;1;0;0;0;0;0;0;0;0|]; 
  [|0;1;1;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]


(* Top three rows of spawning each tetrimino block then moving it right.*)
let i_right_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;1;1;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let l_right_top3 = [|
  [|0;0;0;0;0;0;1;0;0;0|]; 
  [|0;0;0;0;1;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let j_right_top3 = [|
  [|0;0;0;0;1;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let o_right_top3 = [|
  [|0;0;0;0;0;1;1;0;0;0|]; 
  [|0;0;0;0;0;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let s_right_top3 = [|
  [|0;0;0;0;0;1;1;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let t_right_top3 = [|
  [|0;0;0;0;0;1;0;0;0;0|]; 
  [|0;0;0;0;1;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let z_right_top3 = [|
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

(** Top three rows of spawning each tetrimino block then moving it to leftmost.*)
let i_rmost_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;1;1;1;1|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let l_rmost_top3 = [|
  [|0;0;0;0;0;0;0;0;0;1|]; 
  [|0;0;0;0;0;0;0;1;1;1|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let j_rmost_top3 = [|
  [|0;0;0;0;0;0;0;1;0;0|]; 
  [|0;0;0;0;0;0;0;1;1;1|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let o_rmost_top3 = [|
  [|0;0;0;0;0;0;0;0;1;1|]; 
  [|0;0;0;0;0;0;0;0;1;1|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let s_rmost_top3 = [|
  [|0;0;0;0;0;0;0;0;1;1|]; 
  [|0;0;0;0;0;0;0;1;1;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let t_rmost_top3 = [|
  [|0;0;0;0;0;0;0;0;1;0|]; 
  [|0;0;0;0;0;0;0;1;1;1|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let z_rmost_top3 = [|
  [|0;0;0;0;0;0;0;1;1;0|]; 
  [|0;0;0;0;0;0;0;0;1;1|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

(* Top three rows of spawning each tetrimino block then moving it down a row. *)
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

(** Top three rows of spawning each tetrimino block then moving it to leftmost.*)
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

let filled_4 = Array.append (Array.make_matrix 16 10 0) almost_filled

let move_left_tests = [
  grid_test "move spawned i block left" i_left (prepend3 i_left_top3);
  grid_test "move spawned l block left" l_left (prepend3 l_left_top3);
  grid_test "move spawned j block left" j_left (prepend3 j_left_top3);
  grid_test "move spawned o block left" o_left (prepend3 o_left_top3);
  grid_test "move spawned s block left" s_left (prepend3 s_left_top3);
  grid_test "move spawned t block left" t_left (prepend3 t_left_top3);
  grid_test "move spawned z block left" z_left (prepend3 z_left_top3);
  grid_test "move spawned i block lmost" i_leftmost (prepend3 i_lmost_top3);
  grid_test "move spawned l block lmost" l_leftmost (prepend3 l_lmost_top3);
  grid_test "move spawned j block lmost" j_leftmost (prepend3 j_lmost_top3);
  grid_test "move spawned o block lmost" o_leftmost (prepend3 o_lmost_top3);
  grid_test "move spawned s block lmost" s_leftmost (prepend3 s_lmost_top3);
  grid_test "move spawned t block lmost" t_leftmost (prepend3 t_lmost_top3);
  grid_test "move spawned z block lmost" z_leftmost (prepend3 z_lmost_top3);
]

let move_right_tests = [
  grid_test "move spawned i block right" i_right (prepend3 i_right_top3);
  grid_test "move spawned l block right" l_right (prepend3 l_right_top3);
  grid_test "move spawned j block right" j_right (prepend3 j_right_top3);
  grid_test "move spawned o block right" o_right (prepend3 o_right_top3);
  grid_test "move spawned s block right" s_right (prepend3 s_right_top3);
  grid_test "move spawned t block right" t_right (prepend3 t_right_top3);
  grid_test "move spawned z block right" z_right (prepend3 z_right_top3);
  grid_test "move spawned i block rmost" i_rightmost (prepend3 i_rmost_top3);
  grid_test "move spawned l block rmost" l_rightmost (prepend3 l_rmost_top3);
  grid_test "move spawned j block rmost" j_rightmost (prepend3 j_rmost_top3);
  grid_test "move spawned o block rmost" o_rightmost (prepend3 o_rmost_top3);
  grid_test "move spawned s block rmost" s_rightmost (prepend3 s_rmost_top3);
  grid_test "move spawned t block rmost" t_rightmost (prepend3 t_rmost_top3);
  grid_test "move spawned z block rmost" z_rightmost (prepend3 z_rmost_top3);
]

let fall_tests = [
  grid_test "move spawned i block down" i_down (prepend3 i_down_top3);
  grid_test "move spawned l block down" l_down (prepend3 l_down_top3);
  grid_test "move spawned j block down" j_down (prepend3 j_down_top3);
  grid_test "move spawned o block down" o_down (prepend3 o_down_top3);
  grid_test "move spawned s block down" s_down (prepend3 s_down_top3);
  grid_test "move spawned t block down" t_down (prepend3 t_down_top3);
  grid_test "move spawned z block down" z_down (prepend3 z_down_top3);
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
  grid_test "fill first 4 rows" block_4x10 filled_4;
]

let movement_tests = List.flatten [
    move_left_tests;
    move_right_tests;
    fall_tests;
    drop_tests;
  ]

(********************************************************************
   Score Testing
 ********************************************************************)

let score_tests = [
  (* TODO: Add tests for scoring. *)
]

(********************************************************************
   Level and Lines Cleared Testing
 ********************************************************************)

let level_tests = [
  (* TODO: Add tests for level. *)
]

(********************************************************************
   End Helper Suites
 ********************************************************************)

let suite =
  "test suite"  >::: List.flatten [
    spawn_tetromino_tests;
    movement_tests;
    score_tests;
    level_tests;
  ]

let _ = run_test_tt_main suite