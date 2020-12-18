open State
open Tetromino
open Display
open Unix

type t = {
  rotations: int;
  moves_left: int;
  moves_right: int;
}

let initialize rots l r = {
  rotations = rots;
  moves_left = l;
  moves_right = r;
}

let num_rotations m = m.rotations

let num_moves_left m = m.moves_left

let num_moves_right m = m.moves_right

let rotate_n n st =
  for i = 1 to n do st |> State.rotate_cw done

let left_n n st =
  for i = 1 to n do st |> State.move_left done

let right_n n st =
  for i = 1 to n do st |> State.move_right done

let execute st m =
  (* sleepf 0.5; *)
  rotate_n m.rotations st;
  (* sleepf 0.5;
     draw_game_screen st; *)
  left_n m.moves_left st;
  (* sleepf 0.5;
     draw_game_screen st; *)
  right_n m.moves_right st

let grid_after_move st m =
  let st' = copy_grid_and_falling st in
  execute st' m;
  drop ~line_clears: false ~auto_respawn: false st';
  copy_grid_int st'

(** [max_height x 0 grid] is the maximum height of column [x] in [grid]. *)
let rec max_height x y grid =
  match grid.(y).(x) with
  | 1 -> Array.length grid - y
  | _ -> if y = Array.length grid - 1 then 0 else max_height x (y + 1) grid

(** [aggregate_height grid] is the sum of the height of each line in [grid]. *)
let aggregate_height grid = 
  let heights = ref 0 in
  for x = 0 to Array.length grid.(0) - 1 do
    heights := !heights + max_height x 0 grid
  done;
  !heights

(** [is_complete 0 y grid] is true if row [y] of [grid] is complete and false
    otherwise. *)
let rec is_complete x y grid =
  match grid.(y).(x) with
  | 1 -> if x = Array.length grid.(0) - 1 then true 
    else is_complete (x + 1) y grid
  | _ -> false

(** [complete_lines grid] is the number of complete lines in [grid]. *)
let complete_lines grid =
  let lines = ref 0 in
  for y = 0 to Array.length grid - 1 do
    if is_complete 0 y grid then lines := !lines + 1 else ()
  done;
  !lines

(** [is_hole x y grid] is true if position [x], [y] of [grid] is a hole and
    false otherwise. *)
let is_hole x y grid =
  grid.(y).(x) = 0 && grid.(y - 1).(x) = 1

(** [holes grid] is the number of holes in [grid]. A hole is defined by an
    empty square with a full square directly on top of it. *)
let holes grid =
  let holes = ref 0 in
  for x = 0 to Array.length grid.(0) - 1 do
    for y = 1 to Array.length grid - 1 do
      if is_hole x y grid then holes := !holes + 1 else ()
    done;
  done;
  !holes

(** [bumpiness grid] is the variation of column heights in [grid], calculated
    by summing the absolute value of the difference in height of adjacent
    columns. *)
let bumpiness grid =
  let bump = ref 0 in
  for x = 0 to Array.length grid.(0) - 2 do
    bump := !bump + abs((max_height x 0 grid) - (max_height (x + 1) 0 grid))
  done;
  !bump

let move_outcome proj_grid = [
  aggregate_height proj_grid;
  complete_lines proj_grid;
  holes proj_grid;
  bumpiness proj_grid
] |> List.map float_of_int

(** [num_moves num_rot rot_width grid_width] is [(l, r)], where l is the
    number of moves a tetromino can make to the left before hitting the edge
    of the grid, and r is the same, but for the right.
    [num_rot] is the number of times the tetromino has been rotated (0 if not
    at all)
    [rot_width] is the width of the rotated tetromino.
    [grid_width] is the width of the grid that the tetromino is in.
    Raises: Failure if [num_rot] > 3. *)
let num_moves num_rot rot_width grid_width =
  let div = (grid_width - rot_width) / 2 in
  let rem = (grid_width - rot_width) mod 2 in
  match num_rot with
  | 0 
  | 2 -> (div, div + rem)
  | 1 -> (div + rem, div)
  | 3 -> (div - 1, div + rem + 1)
  | _ -> failwith "Too many rotations"


(* [x_width rot] is the width of tetromino "x" when its been rotated [rot]
    times. *)

let i_width = function
  | r when r mod 2 = 0 -> 4
  | _ -> 1

let o_width x = 2

let jlszt_width = function
  | r when r mod 2 = 0 -> 3
  | _ -> 2

(** [possible_moves distinct_rots rot_map grid_width] is a list of moves that
    can be executed on a tetromino with [distinct_rots] distinct rotations in
    a grid [grid]. [rot_map] maps a rotation number to a width. For example,
    the first rotation of an I block maps to 1, since the width of its first
    rotation is 1. *)
let possible_moves distinct_rots rot_map grid_width =
  let sol = ref [] in
  for n_rot = 0 to distinct_rots - 1 do 
    let num_moves = num_moves n_rot (rot_map n_rot) grid_width in
    for left = 0 to fst num_moves do
      for right = 0 to snd num_moves do
        if left = 0 || right = 0
        then sol := (initialize n_rot left right) :: !sol
      done
    done
  done;
  !sol

let get_possible_moves tt grid_width =
  match tt with
  | I_block -> possible_moves 2 i_width grid_width
  | J_block
  | L_block 
  | T_block -> possible_moves 4 jlszt_width grid_width
  | S_block
  | Z_block -> possible_moves 2 jlszt_width grid_width
  | O_block -> possible_moves 1 o_width grid_width
  | Shadow -> failwith "No moves for shadow"