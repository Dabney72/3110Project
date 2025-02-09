open OUnit2
open Move
open Tetromino
open Printers
open State_test

(** [cmp_matrices m1 m2] is true if the matrices m1 and m2 have all the same 
    contents at the same indices and false otherwise. 
    Requires: [m1] and [m2] have the same dimensions. *)
let cmp_matrices m1 m2 =
  let cmpr = ref true in
  for i = 0 to Array.length m1 - 1 do
    for j = 0 to Array.length m1.(0) - 1 do
      cmpr := !cmpr && m1.(i).(j) = m2.(i).(j);
    done;
  done;
  !cmpr

(** [not_redundant move] is whether [move] rotates the piece 0 to 3 times,
    and if [move] either only moves the piece to the left or only moves 
    the piece to the right. *)
let not_redundant move = 
  num_rotations move >= 0 && 
  num_rotations move <= 3 &&
  (num_moves_left move = 0 || 
   num_moves_right move = 0)

(** [possible_moves_test name tt grid_width expected_length] is an OUnit test
    case named [name] for [get_possible_moves tt grid_width], asserting that
    each move in the output is not redundant, and that the length of the output
    is [expected_length]. *)
let possible_moves_test name tt grid_width expected_length =
  name >:: fun _ -> 
    let result = get_possible_moves tt grid_width in
    assert_bool "Error: List contains redundant moves" 
      (List.for_all not_redundant result);
    assert_bool "Error: incorrect number of possible moves" 
      (List.length result = expected_length)

(** [prepend_n] is [arr] with a 10-by-n matrix of zeroes added on top of it. *)
let prepend_n n arr =
  let top = Array.make_matrix n 10 0 in
  Array.append top arr

let grid1 = prepend_n 14 [|
    [|0;0;0;0;1;1;0;0;0;0|];
    [|0;1;1;1;1;1;1;0;0;1|];
    [|0;1;1;0;1;1;1;1;1;1|];
    [|1;1;1;1;1;1;1;1;1;1|];
    [|1;1;1;0;1;1;1;1;1;1|];
    [|1;1;1;1;1;1;1;1;1;1|];
  |]

let grid2 = prepend_n 13 [|
    [|0;0;1;0;0;1;0;0;0;0|];
    [|0;0;1;1;1;1;1;0;0;0|];
    [|0;0;1;1;1;1;1;0;1;1|];
    [|0;0;1;1;1;1;1;0;1;1|];
    [|1;0;1;1;1;1;1;0;1;1|];
    [|1;1;0;1;1;1;1;0;1;1|];
    [|1;1;0;1;1;1;1;0;1;1|];
  |]

let grid3 = [|
  [|1;1;1;1;1;1;1;1;1;1|];
  [|0;0;0;0;0;0;0;0;0;0|];
  [|1;1;1;1;1;1;1;1;1;1|];
  [|0;0;0;0;0;0;0;0;0;0|];
  [|1;1;1;1;1;1;1;1;1;1|];
  [|0;0;0;0;0;0;0;0;0;0|];
  [|1;1;1;1;1;1;1;1;1;1|];
  [|0;0;0;0;0;0;0;0;0;0|];
  [|1;1;1;1;1;1;1;1;1;1|];
  [|0;0;0;0;0;0;0;0;0;0|];
  [|1;1;1;1;1;1;1;1;1;1|];
  [|0;0;0;0;0;0;0;0;0;0|];
  [|1;1;1;1;1;1;1;1;1;1|];
  [|0;0;0;0;0;0;0;0;0;0|];
  [|1;1;1;1;1;1;1;1;1;1|];
  [|0;0;0;0;0;0;0;0;0;0|];
  [|1;1;1;1;1;1;1;1;1;1|];
  [|0;0;0;0;0;0;0;0;0;0|];
  [|1;1;1;1;1;1;1;1;1;1|];
  [|0;0;0;0;0;0;0;0;0;0|];
|]

let grid4 =[|
  [|1;0;1;0;1;0;1;0;1;0|];
  [|0;1;0;1;0;1;0;1;0;1|];
  [|1;0;1;0;1;0;1;0;0;0|];
  [|0;1;0;1;0;1;0;1;0;1|];
  [|1;0;1;0;1;0;1;0;0;0|];
  [|0;1;0;1;0;1;0;1;0;1|];
  [|1;0;1;0;1;0;1;0;1;0|];
  [|0;1;0;1;0;1;0;1;0;1|];
  [|1;0;1;0;1;0;1;0;1;0|];
  [|0;1;0;1;0;1;0;1;0;1|];
  [|1;0;1;0;1;0;1;0;1;0|];
  [|0;1;0;1;0;1;0;1;0;1|];
  [|1;0;1;0;1;0;1;0;1;0|];
  [|0;1;0;1;0;1;0;1;0;1|];
  [|1;0;1;0;1;0;1;0;1;0|];
  [|0;1;0;1;0;1;0;1;0;1|];
  [|1;0;1;0;1;0;1;0;1;0|];
  [|0;1;0;1;0;1;0;1;0;1|];
  [|0;0;1;0;1;0;1;0;1;0|];
  [|0;1;0;1;0;1;0;1;0;1|];
|]

(* gamt_grids are grids use to test the output for the various
   grid_after_move_tests. *)
let gamt_grid1 = prepend_n 19 [|
    [|0;0;0;1;1;1;1;0;0;0|];
  |]

let gamt_grid2 = prepend_n 18 [|
    [|0;0;1;1;0;0;0;0;0;0|];
    [|0;0;1;1;0;0;0;0;0;0|];
  |]

let gamt_grid3 = prepend_n 18 [|
    [|0;0;0;0;0;0;0;1;1;0|];
    [|0;0;0;0;0;0;1;1;0;0|];
  |]

let gamt_grid4 = prepend_n 17 [|
    [|0;0;0;0;1;0;0;0;0;0|];
    [|0;0;0;0;1;0;0;0;0;0|];
    [|0;0;0;0;1;1;0;0;0;0|];
  |]

let gamt_grid5 = prepend_n 18 [|
    [|0;0;0;0;1;1;1;0;0;0|];
    [|0;0;0;0;0;1;0;0;0;0|];
  |]

let gamt_grid6 = prepend_n 18 [|
    [|0;0;0;0;0;0;0;1;1;0|];
    [|1;1;1;1;1;1;1;1;1;1|];
  |]

let gamt_grid7 = prepend_n 16 [|
    [|1;1;1;1;1;1;1;1;1;1|];
    [|1;1;1;1;1;1;1;1;1;1|];
    [|1;1;1;1;1;1;1;1;1;1|];
    [|1;1;1;1;1;1;1;1;1;1|];
  |]

let init_with_block block = State.initialize ~first_block: (Some block) ()

let first_8 = 
  initial ()
  |> spawn_move_drop I_block 0 4 0
  |> spawn_move_drop I_block 0 0 1
  |> spawn Z_block

let almost_tetris =
  initial ()
  |> spawn_move_drop_n I_block 0 0 3 4
  |> spawn_move_drop_n I_block 0 1 0 4
  |> spawn_move_drop I_block 1 4 0
  |> spawn I_block

let filled_5 = prepend_n 15 (Array.make_matrix 5 10 1)

let move_tests = [
  possible_moves_test "I Block - 17 possible moves"
    I_block 10 17;
  possible_moves_test "L Block - 34 possible moves"
    L_block 10 34;
  possible_moves_test "J Block - 34 possible moves"
    J_block 10 34;
  possible_moves_test "O Block - 9 possible moves"
    O_block 10 9;
  possible_moves_test "S Block - 17 possible moves"
    S_block 10 17;
  possible_moves_test "Z Block - 17 possible moves"
    Z_block 10 17;
  possible_moves_test "T Block - 34 possible moves"
    T_block 10 34;
]

let grid_after_move_test name output st move =
  name >:: fun _ -> 
    assert_equal ~cmp:cmp_matrices ~printer: pp_int_matrix
      output (grid_after_move st move)

let aggregate_height_test name output grid =
  name >:: fun _ -> assert_equal output (aggregate_height grid) 
      ~printer: string_of_int

let complete_lines_test name output grid =
  name >:: fun _ -> assert_equal output (complete_lines grid) 
      ~printer: string_of_int

let holes_test name output grid =
  name >:: fun _ -> assert_equal output (holes grid) 
      ~printer: string_of_int

let bumpiness_test name output grid =
  name >:: fun _ -> assert_equal output (bumpiness grid) 
      ~printer: string_of_int

let grid_tests = [
  grid_after_move_test "no moves for I block" gamt_grid1
    (init_with_block I_block) (initialize 0 0 0);
  grid_after_move_test "4 rotations 3 moves left and 3 moves right for I block"
    gamt_grid1 (init_with_block I_block) (initialize 4 3 3);
  grid_after_move_test "2 moves left for O block" gamt_grid2
    (init_with_block O_block) (initialize 0 2 0);
  grid_after_move_test "3 moves right for S Block" gamt_grid3
    (init_with_block S_block) (initialize 0 0 3);
  grid_after_move_test "1 rotation for L Block" gamt_grid4
    (init_with_block L_block) (initialize 1 0 0);
  grid_after_move_test "2 rotations and 1 move right for T Block" gamt_grid5
    (init_with_block T_block) (initialize 2 0 1);
  grid_after_move_test "Clear line with Z block" gamt_grid6
    first_8 (initialize 0 0 5);
  grid_after_move_test "Tetris with I block" gamt_grid7
    almost_tetris (initialize 1 5 0);
  aggregate_height_test "empty grid heights" 0 (Array.make_matrix 20 10 0);
  aggregate_height_test "full grid heights" 200 (Array.make_matrix 20 10 1);
  aggregate_height_test "grid1 heights" 48 grid1;
  aggregate_height_test "grid2 heights" 47 grid2;
  aggregate_height_test "grid3 heights" 200 grid3;
  aggregate_height_test "grid4 heights" 195 grid4;
  complete_lines_test "empty grid lines" 0 (Array.make_matrix 20 10 0);
  complete_lines_test "full grid lines" 20 (Array.make_matrix 20 10 1);
  complete_lines_test "grid1 lines" 2 grid1;
  complete_lines_test "grid2 lines" 0 grid2;
  complete_lines_test "grid3 lines" 10 grid3;
  complete_lines_test "grid4 lines" 0 grid4;
  holes_test "empty grid holes" 0 (Array.make_matrix 20 10 0);
  holes_test "full grid holes" 0 (Array.make_matrix 20 10 1);
  holes_test "grid1 holes" 2 grid1;
  holes_test "grid2 holes" 2 grid2;
  holes_test "grid3 holes" 100 grid3;
  holes_test "grid4 holes" 98 grid4;
  bumpiness_test "empty grid bumpiness" 0 (Array.make_matrix 20 10 0);
  bumpiness_test "full grid bumpiness" 0 (Array.make_matrix 20 10 1);
  bumpiness_test "grid1 bumpiness" 6 grid1;
  bumpiness_test "grid2 bumpiness" 20 grid2;
  bumpiness_test "grid3 bumpiness" 0 grid3;
  bumpiness_test "grid4 bumpiness" 9 grid4;
]

let suite =
  "Move test suite"  >::: List.flatten [
    move_tests;
    grid_tests;
  ]

let _ = print_newline (); print_endline "Running AI Move Tests..."
let _ = run_test_tt_main suite