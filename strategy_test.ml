open OUnit2
open Strategy
open Printers

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
    [name] for [crossover s1 s2 f1 f2], ensuring that the output is
    [expected]. *)
let crossover_test name s1 s2 f1 f2 expected =
  name >:: fun _ -> 
    assert_equal 
      (expected |> to_list) 
      (crossover s1 s2 f1 f2 |> to_list)
      ~printer: (pp_list string_of_float)
      ~cmp: cmp_float_lists

let s1 = init_with_weights 0.0 0.8 0.0 0.6
let s2 = init_with_weights 0.5 0.5 0.5 0.5
let s1_cross_s2 = init_with_weights 1.0 3.4 1.0 2.8

let s3 = init_with_weights 0.3 (-0.1) 0.4 (-0.6)
let s1_cross_s3 = init_with_weights 0.3 1.5 0.4 0.6
let s2_cross_s3 = init_with_weights (-0.9) (-1.7) (-0.7) (-2.7)


let strategy_tests = [
  init_test "Test initialize";
  mutate_test "Ensure mutate maintains bounds";
  crossover_test "s1 and s2" s1 s2 3.0 2.0 s1_cross_s2;
  crossover_test "s1 and s3" s1 s3 2.0 1.0 s1_cross_s3;
  crossover_test "s2 and s3" s2 s3 (-3.0) 2.0 s2_cross_s3;
]

let suite =
  "Strategy test suite"  >::: strategy_tests

let _ = print_newline (); print_endline "Running AI Strategy Tests..."
let _ = run_test_tt_main suite