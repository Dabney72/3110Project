open OUnit2

let suite =
  "Strategies test suite"  >::: List.flatten []

let _ = print_newline (); print_endline "Running AI Strategies Tests..."
let _ = run_test_tt_main suite