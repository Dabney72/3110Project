open OUnit2
open State
open Tetromino

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
    assert_equal (grid ()) (spawn_tetromino tetromino st; get_grid st) 
      ~printer: pp_matrix

(** [grid_test name st grid] is an OUnit test case named [name] for 
    [st] asserting that its grid attribute is [grid]. *)
let grid_test name st grid =
  name >:: fun ctxt ->  
    assert_equal grid (get_grid st) ~printer: pp_matrix

(********************************************************************
   Initilization and Spawn Testing
 ********************************************************************)

(* Matrix creation *)
let initial = State.initialize
let tenbyseventeen = Array.make_matrix 17 10 0
let create_10x20 top3 = Array.append top3 tenbyseventeen 

(* Top three rows of each tetromino type. *)
let i_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let l_top3 = [|
  [|0;0;0;1;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let j_top3 = [|
  [|0;0;0;0;0;1;0;0;0;0|]; 
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
let i_grid () = create_10x20 i_top3
let l_grid () = create_10x20 l_top3
let j_grid () = create_10x20 j_top3 
let o_grid () = create_10x20 o_top3 
let s_grid () = create_10x20 s_top3 
let t_grid () = create_10x20 t_top3 
let z_grid () = create_10x20 z_top3 

(* Initialize tetromino types*)
let init_tetr tetromino = Tetromino.init_tetromino tetromino
let i_block = init_tetr I_block
let l_block = init_tetr L_block
let j_block = init_tetr J_block
let o_block = init_tetr O_block
let s_block = init_tetr S_block
let t_block = init_tetr T_block
let z_block = init_tetr Z_block

let spawn_tetromino_tests = [
  spawn_tetromino_test "spawn iblock" (initial ()) i_block i_grid;
  spawn_tetromino_test "spawn lblock" (initial ()) l_block l_grid;
  spawn_tetromino_test "spawn jblock" (initial ()) j_block j_grid;
  spawn_tetromino_test "spawn oblock" (initial ()) o_block o_grid;
  spawn_tetromino_test "spawn sblock" (initial ()) s_block s_grid;
  spawn_tetromino_test "spawn tblock" (initial ()) t_block t_grid;
  spawn_tetromino_test "spawn zblock" (initial ()) z_block z_grid;
]

(********************************************************************
   Movement Testing
 ********************************************************************)

(* Helper functions *)
let spawn block st = st |> spawn_tetromino block; st
let block_left block st =  st |> spawn block |> move_left; st
let block_right block st =  st |> spawn block |> move_right; st
let block_down block st =  st |> spawn block |> fall; st

(* Spawn block, move it left then return the state.*)
let i_left = initial () |> block_left i_block
let l_left = initial () |> block_left l_block
let j_left = initial () |> block_left j_block
let o_left = initial () |> block_left o_block
let s_left = initial () |> block_left s_block
let t_left = initial () |> block_left t_block
let z_left = initial () |> block_left z_block

(* Spawn block, move it right then return the state.*)
let i_right = initial () |> block_right i_block
let l_right = initial () |> block_right l_block
let j_right = initial () |> block_right j_block
let o_right = initial () |> block_right o_block
let s_right = initial () |> block_right s_block
let t_right = initial () |> block_right t_block
let z_right = initial () |> block_right z_block

(* Spawn block, fall down one row then return the state.*)
let i_down = initial () |> block_down i_block
let l_down = initial () |> block_down l_block
let j_down = initial () |> block_down j_block
let o_down = initial () |> block_down o_block
let s_down = initial () |> block_down s_block
let t_down = initial () |> block_down t_block
let z_down = initial () |> block_down z_block


(* Top three rows of spawning each tetrimno block then moving it left.*)
let i_left_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;1;1;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let l_left_top3 = [|
  [|0;0;1;0;0;0;0;0;0;0|]; 
  [|0;0;1;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let j_left_top3 = [|
  [|0;0;0;0;1;0;0;0;0;0|]; 
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

(* Top three rows of spawning each tetrimno block then moving it right.*)
let i_right_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;1;1;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let l_right_top3 = [|
  [|0;0;0;0;1;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let j_right_top3 = [|
  [|0;0;0;0;0;0;1;0;0;0|]; 
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

(* Top three rows of each tetromino type. *)
let i_down_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;1;0;0;0|]
|]
let l_down_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;1;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]
|]
let j_down_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;0;0;1;0;0;0;0|]; 
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

let move_left_tests = [
  grid_test "move spawned i block left" i_left (create_10x20 i_left_top3);
  grid_test "move spawned l block left" l_left (create_10x20 l_left_top3);
  grid_test "move spawned j block left" j_left (create_10x20 j_left_top3);
  grid_test "move spawned o block left" o_left (create_10x20 o_left_top3);
  grid_test "move spawned s block left" s_left (create_10x20 s_left_top3);
  grid_test "move spawned t block left" t_left (create_10x20 t_left_top3);
  grid_test "move spawned z block left" z_left (create_10x20 z_left_top3);
]

let move_right_tests = [
  grid_test "move spawned i block right" i_right (create_10x20 i_right_top3);
  grid_test "move spawned l block right" l_right (create_10x20 l_right_top3);
  grid_test "move spawned j block right" j_right (create_10x20 j_right_top3);
  grid_test "move spawned o block right" o_right (create_10x20 o_right_top3);
  grid_test "move spawned s block right" s_right (create_10x20 s_right_top3);
  grid_test "move spawned t block right" t_right (create_10x20 t_right_top3);
  grid_test "move spawned z block right" z_right (create_10x20 z_right_top3);
]

let fall_tests = [
  grid_test "move spawned i block down" i_down (create_10x20 i_down_top3);
  grid_test "move spawned l block down" l_down (create_10x20 l_down_top3);
  grid_test "move spawned j block down" j_down (create_10x20 j_down_top3);
  grid_test "move spawned o block down" o_down (create_10x20 o_down_top3);
  grid_test "move spawned s block down" s_down (create_10x20 s_down_top3);
  grid_test "move spawned t block down" t_down (create_10x20 t_down_top3);
  grid_test "move spawned z block down" z_down (create_10x20 z_down_top3);
]

let movement_tests = List.flatten [
    move_left_tests;
    move_right_tests;
    fall_tests;
  ]

(********************************************************************
   End Helper Suites
 ********************************************************************)

let suite =
  "test suite"  >::: List.flatten [
    spawn_tetromino_tests;
    movement_tests;
  ]

let _ = run_test_tt_main suite