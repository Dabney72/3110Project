open OUnit2

let suite =
  "Strategy test suite"  >::: List.flatten []

let _ = print_newline (); print_endline "Running AI Strategy Tests..."
let _ = run_test_tt_main suite