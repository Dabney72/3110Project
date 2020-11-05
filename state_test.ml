open OUnit2
open State

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
    [name] for [spawn_tetromino st tetromino] asserting that the output 
    is [grid]. *)
let spawn_tetromino_test name st tetromino grid =
  name >:: fun ctxt ->  
    assert_equal grid (spawn_tetromino st tetromino; get_grid st) 
      ~printer: pp_matrix

(********************************************************************
   End helper functions.
 ********************************************************************)

let initial = State.initialize
let tenbyseventeen = Array.make_matrix 17 10 0
let create_10x20 top3 = Array.append top3 tenbyseventeen 
let iblock_top3 = [|
  [|0;0;0;0;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;1;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let lblock_top3 = [|
  [|0;0;0;1;0;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let jblock_top3 = [|
  [|0;0;0;0;0;1;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let oblock_top3 = [|
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let sblock_top3 = [|
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let tblock_top3 = [|
  [|0;0;0;0;1;0;0;0;0;0|]; 
  [|0;0;0;1;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]
let zblock_top3 = [|
  [|0;0;0;1;1;0;0;0;0;0|]; 
  [|0;0;0;0;1;1;0;0;0;0|]; 
  [|0;0;0;0;0;0;0;0;0;0|]
|]

let iblock_grid = create_10x20 iblock_top3
let lblock_grid = create_10x20 lblock_top3
let jblock_grid = create_10x20 jblock_top3 
let oblock_grid = create_10x20 oblock_top3 
let sblock_grid = create_10x20 sblock_top3 
let tblock_grid = create_10x20 tblock_top3 
let zblock_grid = create_10x20 zblock_top3 


let spawn_tetromino_tests = [
  spawn_tetromino_test "spawn iblock" (initial ()) 
    (Tetromino.init_tetromino I_block) iblock_grid;
  spawn_tetromino_test "spawn lblock" (initial ()) 
    (Tetromino.init_tetromino L_block) lblock_grid;
  spawn_tetromino_test "spawn jblock" (initial ())
    (Tetromino.init_tetromino J_block) jblock_grid;
  spawn_tetromino_test "spawn oblock" (initial ())
    (Tetromino.init_tetromino O_block) oblock_grid;
  spawn_tetromino_test "spawn sblock" (initial ())
    (Tetromino.init_tetromino S_block) sblock_grid;
  spawn_tetromino_test "spawn tblock" (initial ())
    (Tetromino.init_tetromino T_block) tblock_grid;
  spawn_tetromino_test "spawn zblock" (initial ())
    (Tetromino.init_tetromino Z_block) zblock_grid;
]

let suite =
  "test suite"  >::: List.flatten [
    spawn_tetromino_tests;
  ]

let _ = run_test_tt_main suite