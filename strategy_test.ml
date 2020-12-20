open OUnit2
open Strategy
open Printers
open State_test

(** [bounded_by bound lst] is true if all elements in [lst] are in the range
    given by +-[bound]. *)
let bounded_by bound =
  List.for_all (fun x -> x <= bound && x >= -.bound)

(** [cmp_float_lists lst1 lst2] is whether each of the entries [lst1] and
    [lst2] are equal to a precision of .0001. *)
let cmp_float_lists =
  List.for_all2 (cmp_float ~epsilon: 0.0001)

(** [init_test name] is an OUnit test case named [name], ensuring that
    [initialize ()] produces a vector of floats all between +- 1.0. *)
let init_test name =
  name >:: fun _ -> assert_bool "Weights not bounded after initialization"
      (bounded_by 1.0 (initialize () |> to_list))

(** [mutate_test name] is an OUnit test case named [name], ensuring that 
    mutating some random strategy keeps all entries <= 3.0 in absolute value. *)
let mutate_test name =
  name >:: fun _ -> assert_bool "Mutation violates bounds" 
      (bounded_by 3.0 (mutate (initialize ()) |> to_list))

(** [crossover_test name s1 s2 f1 f2 expected] is an OUnit test case named 
    [name] for [crossover s1 s2 f1 f2], asserting that the output is
    [expected]. *)
let crossover_test name s1 s2 f1 f2 expected =
  name >:: fun _ -> 
    assert_equal 
      (expected |> to_list) 
      (crossover s1 s2 f1 f2 |> to_list)
      ~printer: (pp_list string_of_float)
      ~cmp: cmp_float_lists

let cmp_last_18 arr1 arr2 =
  Array.(sub arr1 2 18 = sub arr2 2 18)

(** [move_next_piece_test name s st expected] is an OUnit test case named 
    [name] for [move_next_piece name s st expected], asserting that the
    grid in [st] is [expected]. *)
let move_next_piece_test name s st expected =
  name >:: fun _ -> 
    assert_equal expected 
      (let st' = State.copy_grid_and_falling st in
       (move_next_piece s st'; State.copy_grid_int st'))
      ~printer: pp_int_matrix
      ~cmp: cmp_last_18

let s1 = init_with_weights 0.0 0.8 0.0 0.6
let s2 = init_with_weights 0.5 0.5 0.5 0.5
let s3 = init_with_weights 0.3 (-0.1) 0.4 (-0.6)
let s4 = init_with_weights (-0.2) 3.2 (-2.4) (-1.5)

let s1_cross_s2 = init_with_weights 1.0 3.4 1.0 2.8
let s1_cross_s3 = init_with_weights 0.3 1.5 0.4 0.6
let s2_cross_s3 = init_with_weights (-0.9) (-1.7) (-0.7) (-2.7)
let s1_cross_s4 = init_with_weights (-0.6) 12.0 (-7.2) (-2.7)
let s3_cross_s4 = init_with_weights 1.36 7.38 (-3.52) (-7.47)

let agg_height = init_with_weights (-1.) 0. 0. 0.
let clear_line = init_with_weights 0. 1. 0. 0.
let holes = init_with_weights 0. 0. (-1.) 0.
let bumpiness = init_with_weights 0. 0. 0. (-1.)

let clear_line_st = 
  initial ()
  |> spawn_move_drop O_block 0 4 0
  |> spawn_move_drop O_block 0 2 0
  |> spawn_move_drop O_block 0 0 0
  |> spawn_move_drop O_block 0 0 2
  |> spawn O_block

let i_block_st =
  initial () |> spawn I_block

let t_block_st =
  initial () |> spawn T_block

let o_block_st =
  initial () |> spawn O_block

let almost_tetris =
  State.copy_grid_and_falling block_4x9 |> spawn I_block

let empty_grid = Array.make_matrix 20 10 0

let prepend_n n arr =
  let top = Array.make_matrix n 10 0 in
  Array.append top arr

let i_block_corner = prepend_n 16 [|
    [|1;0;0;0;0;0;0;0;0;0|];
    [|1;0;0;0;0;0;0;0;0;0|];
    [|1;0;0;0;0;0;0;0;0;0|];
    [|1;0;0;0;0;0;0;0;0;0|];
  |]

let i_block_corner_rot = prepend_n 19 [|
    [|1;1;1;1;0;0;0;0;0;0|];
  |]

let t_block_corner = prepend_n 18 [|
    [|0;1;0;0;0;0;0;0;0;0|];
    [|1;1;1;0;0;0;0;0;0;0|];
  |]

let t_block_corner_rot = prepend_n 18 [|
    [|1;1;1;0;0;0;0;0;0;0|];
    [|0;1;0;0;0;0;0;0;0;0|];
  |]

let o_block_corner = prepend_n 18 [|
    [|1;1;0;0;0;0;0;0;0;0|];
    [|1;1;0;0;0;0;0;0;0;0|];
  |]

let tetris_fail = prepend_n 12 [|
    [|1;0;0;0;0;0;0;0;0;0|]; 
    [|1;0;0;0;0;0;0;0;0;0|]; 
    [|1;0;0;0;0;0;0;0;0;0|]; 
    [|1;0;0;0;0;0;0;0;0;0|]; 
    [|1;1;1;1;1;1;1;1;1;0|]; 
    [|1;1;1;1;1;1;1;1;1;0|]; 
    [|1;1;1;1;1;1;1;1;1;0|]; 
    [|1;1;1;1;1;1;1;1;1;0|];
  |]

let strategy_tests = [
  init_test "Test initialize";
  mutate_test "Ensure mutate maintains bounds";
  crossover_test "s1 and s2" s1 s2 3.0 2.0 s1_cross_s2;
  crossover_test "s1 and s3" s1 s3 2.0 1.0 s1_cross_s3;
  crossover_test "s2 and s3" s2 s3 (-3.0) 2.0 s2_cross_s3;
  crossover_test "s1 and s4" s1 s4 3.0 3.0 s1_cross_s4;
  crossover_test "s3 and s4" s3 s4 6.2 2.5 s3_cross_s4;
  move_next_piece_test "Clear line strategy clears line" 
    clear_line clear_line_st empty_grid;
  move_next_piece_test "Clear line strategy moves I block left" 
    clear_line i_block_st i_block_corner;
  move_next_piece_test "Bumpiness strategy rotates I block"
    bumpiness i_block_st i_block_corner_rot;
  move_next_piece_test "Holes strategy moves I block left"
    holes i_block_st i_block_corner;
  move_next_piece_test "Agg height strategy moves T block left"
    agg_height t_block_st t_block_corner;
  move_next_piece_test "Holes strategy moves T block left"
    holes t_block_st t_block_corner;
  move_next_piece_test "Bumpiness strategy rotates and moves T block left"
    bumpiness t_block_st t_block_corner_rot;
  move_next_piece_test "Agg height strategy moves O block left"
    agg_height o_block_st o_block_corner;
  move_next_piece_test "Clear line strategy moves O block left"
    clear_line o_block_st o_block_corner;
  move_next_piece_test "Holes strategy moves O block left"
    holes o_block_st o_block_corner;
  move_next_piece_test "Bumpiness strategy moves O block left"
    bumpiness o_block_st o_block_corner;
  move_next_piece_test "Clear line strategy clears 4 lines" 
    clear_line almost_tetris empty_grid;
  move_next_piece_test "Bumpiness strategy clears 4 lines" 
    bumpiness almost_tetris empty_grid;
  move_next_piece_test "Holes strategy doesn't clear 4 lines" 
    holes almost_tetris tetris_fail;
  move_next_piece_test "Agg height strategy doesn't clear 4 lines" 
    agg_height almost_tetris tetris_fail;
]

let suite =
  "Strategy test suite"  >::: strategy_tests

let _ = print_newline (); print_endline "Running AI Strategy Tests..."
let _ = run_test_tt_main suite