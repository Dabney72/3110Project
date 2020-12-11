open OUnit2
open Move
open Tetromino
open Printers

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

(** [outcome_test name proj_grid expected] is an OUnit test case named [name]
    for [move_outcome proj_grid], asserting that the output is [expected]. *)
let outcome_test name proj_grid expected =
  name >:: fun _ ->
    assert_equal expected (move_outcome proj_grid)
      ~printer: (pp_list string_of_float)

(** [grid_after_move_test name st move expected] is an OUnit test case named
    [name] for [grid_after_move st move], asserting that the output is
    [expected]. *)
let grid_after_move_test name st move expected =
  name >:: fun _ -> 
    assert_equal expected (grid_after_move st move)
    ~printer: pp_int_matrix

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
    [|1;1;1;1;1;1;1;0;1;1|];
    [|1;1;0;1;1;1;1;0;1;1|];
  |]

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

(* TODO: Implement the four outcome functions (aggregate_height, complete_lines,
  holes, bumpiness) in move.ml, and make sure these test cases pass (if they're
  not passing and you think they should, it's possible that I got the numbers
  wrong, but I think I inserted the outputs correctly). *)
let outcome_tests = [
  (* outcome_test "First 5 filled" filled_5 [50.0; 5.0; 0.0; 0.0]; *)
  (* outcome_test "Grid 1" grid1 [48.0; 2.0; 2.0; 6.0]; *)
  (* outcome_test "Grid 2" grid2 [47.0; 0.0; 1.0; 20.0]; *)
]

(* TODO: Add tests for grid_after_move  *)
let grid_tests = [

]

let suite =
  "Move test suite"  >::: List.flatten [
    move_tests;
    outcome_tests;
    grid_tests;
  ]

let _ = print_newline (); print_endline "Running AI Move Tests..."
let _ = run_test_tt_main suite