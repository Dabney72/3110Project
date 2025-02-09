open OUnit2
open Tetromino
open Printers

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [compare_tetrominoes] is true iff [t1] and [t2] have the same composition
    and width. *)
let compare_tetrominoes t1 t2 =
  cmp_set_like_lists (get_comp t1) (get_comp t2)
  && 
  get_width t1 = get_width t2

(** [rotate_test rotator name tetromino expected] is an OUnit test case named
    [name] for [rotator tetromino], asserting that the output is [expected]. *)
let rotate_test rotator name tetromino expected =
  name >:: (fun _ -> assert_equal expected (rotator tetromino)
               ~printer: pp_tetromino
               ~cmp: compare_tetrominoes)

(* 7 standard tetrominoes *)
let i_block = init_tetromino I_block
let l_block = init_tetromino L_block
let j_block = init_tetromino J_block
let o_block = init_tetromino O_block
let s_block = init_tetromino S_block
let t_block = init_tetromino T_block
let z_block = init_tetromino Z_block

(* Rotations of each tetromino *)
let i1 = create_tetromino [(2,0); (2,1); (2,2); (2,3)] 4
let i2 = create_tetromino [(0,2); (1,2); (2,2); (3,2)] 4
let i3 = create_tetromino [(1,0); (1,1); (1,2); (1,3)] 4

let l1 = create_tetromino [(1,0); (1,1); (1,2); (2,2)] 3
let l2 = create_tetromino [(0,1); (1,1); (2,1); (0,2)] 3
let l3 = create_tetromino [(0,0); (1,0); (1,1); (1,2)] 3

let j1 = create_tetromino [(1,0); (1,1); (1,2); (2,0)] 3
let j2 = create_tetromino [(0,1); (1,1); (2,1); (2,2)] 3
let j3 = create_tetromino [(0,2); (1,0); (1,1); (1,2)] 3

let s1 = create_tetromino [(1,0); (1,1); (2,1); (2,2)] 3
let s2 = create_tetromino [(0,2); (1,2); (1,1); (2,1)] 3
let s3 = create_tetromino [(0,0); (0,1); (1,1); (1,2)] 3

let t1 = create_tetromino [(1,0); (1,1); (1,2); (2,1)] 3
let t2 = create_tetromino [(0,1); (1,1); (2,1); (1,2)] 3
let t3 = create_tetromino [(0,1); (1,0); (1,1); (1,2)] 3

let z1 = create_tetromino [(2,0); (2,1); (1,1); (1,2)] 3
let z2 = create_tetromino [(0,1); (1,1); (1,2); (2,2)] 3
let z3 = create_tetromino [(1,0); (1,1); (0,1); (0,2)] 3

(** [test_rotate t_type one_rot two_rot three_rot] is a list of OUnit tests
    for each rotation of the tetromino of type [t_type]. First, the clockwise
    rotations are tested against [one_rot] for the first rotation,
    [two_rot] for the second, and [three_rot] for the third.
    The fourth rotation should yield the original tetromino. The process is
    then repeated again for counterclockwise rotations. *)
let test_rotate t_type one_rot two_rot three_rot = 
  let tetr = init_tetromino t_type in [
    rotate_test rotate_cw ("Rotate " ^ get_name t_type ^ " cw once")
      tetr one_rot;
    rotate_test rotate_cw ("Rotate " ^ get_name t_type ^ " cw twice")
      one_rot two_rot;
    rotate_test rotate_cw ("Rotate " ^ get_name t_type ^ " cw three times")
      two_rot three_rot;
    rotate_test rotate_cw ("Rotate " ^ get_name t_type ^ " cw four times")
      three_rot tetr;
    rotate_test rotate_ccw ("Rotate " ^ get_name t_type ^ " ccw once")
      tetr three_rot;
    rotate_test rotate_ccw ("Rotate " ^ get_name t_type ^ " ccw twice")
      three_rot two_rot;
    rotate_test rotate_ccw ("Rotate " ^ get_name t_type ^ " ccw three times")
      two_rot one_rot;
    rotate_test rotate_ccw ("Rotate " ^ get_name t_type ^ " ccw four times")
      one_rot tetr;
  ]

let rotate_tests = List.flatten [
    test_rotate I_block i1 i2 i3;
    test_rotate L_block l1 l2 l3;
    test_rotate J_block j1 j2 j3;
    test_rotate O_block o_block o_block o_block;
    test_rotate S_block s1 s2 s3;
    test_rotate T_block t1 t2 t3;
    test_rotate Z_block z1 z2 z3;
  ]
let a  = generate_list ()
let b = generate_list ()
let c = generate_list ()

let gen_test = [
  "Generate list a has all 7 blocks" >:: 
  (fun _ -> assert_equal 7 (List.sort_uniq compare a |> List.length) 
      ~printer: (string_of_int));
  "Generate list b has all 7 blocks" >:: 
  (fun _ -> assert_equal 7 (List.sort_uniq compare b |> List.length) 
      ~printer: (string_of_int));
  "Generate list c has all 7 blocks" >:: 
  (fun _ -> assert_equal 7 (List.sort_uniq compare c |> List.length) 
      ~printer: (string_of_int));
] 

let suite =
  "Tetromino test suite"  >::: List.flatten [
    rotate_tests;
    gen_test;
  ]

let _ = print_newline (); print_endline "Running Tetromino Tests..."
let _ = run_test_tt_main suite