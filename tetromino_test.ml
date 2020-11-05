open OUnit2
open Tetromino

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [string_of_ints (x, y)] is a string representation of the tuple of ints 
    [(x, y)]. *)
let string_of_ints (x, y) =
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

(** [print_tetr t] is a string representation of the tetromino [t]. *)
let print_tetr t =
  t |> get_comp |> pp_list string_of_ints

(* let compare_tetrominos t1 t2 =
  get_comp t1 = get_comp t2 && get_width t1 = get_width t2 *)

(** [rotate_test name tetromino expected] is an OUnit test case named [name]
    for [rotate tetromino], asserting that the output is [expected]. *)
let rotate_test name tetromino expected =
  name >:: (fun _ -> assert_equal expected (rotate tetromino)
               ~printer: (print_tetr))

(* 7 standard tetrominoes *)
let i_block = init_tetromino I_block
let l_block = init_tetromino L_block
let j_block = init_tetromino J_block
let o_block = init_tetromino O_block
let s_block = init_tetromino S_block
let t_block = init_tetromino T_block
let z_block = init_tetromino Z_block

(* Rotations of each tetromino *)
let i_block_1_rot = create_tetromino [(2,0); (2,1); (2,2); (2,3)] 4
let i_block_2_rot = create_tetromino [(0,2); (1,2); (2,2); (3,2)] 4

let rotate_tests = [
  rotate_test "rotate I block once" 
    i_block i_block_1_rot;
  rotate_test "rotate I block twice" 
    i_block_1_rot i_block_2_rot;
  (* rotate_test "rotate I block three times" 
    i_block_2_rot i_block_3_rot;
  rotate_test "rotate I block four times" 
    i_block_3_rot i_block; *)

]

let suite =
  "Tetromino test suite"  >::: List.flatten [
    rotate_tests;
  ]

let _ = run_test_tt_main suite