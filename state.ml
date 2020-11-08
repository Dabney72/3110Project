open Tetromino

type direction = 
  | Left
  | Right
  | Down

type falling = {
  mutable block : Tetromino.t;
  mutable pos : (int * int);
}

type t = {
  mutable grid : int array array;
  mutable falling_block : falling option;
  mutable upcoming_blocks : tetromino_type list;
  mutable held_block : tetromino_type option;
  mutable score : int
}

let initialize () =
  {
    grid = Array.make_matrix 20 10 0;
    falling_block = None;
    upcoming_blocks = generate_list ();
    held_block = None;
    score = 0;
  }

let get_grid st =
  st.grid

let get_score t =
  t.score

let get_hold t =
  t.held_block

let get_upcoming t =
  t.upcoming_blocks

let rotate st =
  failwith "Unimplemented"

(** [get_falling st] is the current block that is falling.
    Raises: Failure if no block is currently falling. *)
let get_falling st = 
  match st.falling_block with
  | None -> failwith "No falling block"
  | Some f -> f

(** [optimum coord cmp comp] is the most extreme coordinate in [comp].
    [coord] is a function that tells which coordinate to optimize (e.g. 
    fst for x or snd for y), and [cmp a b] is true if a is more extreme than b. 
    Ex: [optimum fst (>) [(2,0);(1,3);(3,1)]] is 3, the largest x-coordinate
    in the given list. 
    Raises: Failure if [comp] is empty. *)
let rec optimum coord cmp = function 
  | [] -> failwith "empty"
  | h :: [] -> coord h
  | h :: t -> 
    let opt_tail = optimum coord cmp t in
    let current_val = coord h in
    if cmp current_val opt_tail then current_val else opt_tail

(** [falling_block_coords st] is a list of (row, column) coordinates for the 
    current falling block. *)
let falling_block_coords st =
  let falling = get_falling st in
  let rec help falling comp =
    match comp with
    | [] -> []
    | (y, x) :: t -> 
      match falling.pos with
      | (r, c) -> (r + x, c + y) :: help falling t
  in help falling (get_comp falling.block)

(** [collision st block_bound grid_bound bound_reached] is true if [block_bound]
    has reached [grid_bound], or if [block_bound] has reached another block,
    determined by the function [bound_reached]. *)
let collision st block_bound grid_bound bound_reached =
  let coords = falling_block_coords st in
  let rec check_coords st = function
    | [] -> false
    | (r, c) :: t -> 
      block_bound = grid_bound (* Checking out of bounds *)
      || bound_reached st.grid r c block_bound
      || check_coords st t
  in check_coords st coords

(** [block_at_row_bound grid _ c row_bound] is true if the grid has a block
    at [(row_bound, c)]. *)
let block_at_row_bound grid _ c row_bound =
  grid.(row_bound).(c) = 1

(** [block_at_col_bound grid r _ col_bound] is true if the grid has a block
    at [(r, col_bound)]. *)
let block_at_col_bound grid r _ col_bound =
  grid.(r).(col_bound) = 1

(** [collision_under st] is true iff there is a block under the currently
    falling block, or if the falling block has reached the bottom of the
    grid. *)
let collision_under st =
  let bottom_row = optimum fst (>) (falling_block_coords st) in
  let num_rows = Array.length st.grid in
  collision st (bottom_row + 1) num_rows block_at_row_bound

(** [collision_left st] is true iff there is a block to the left the currently
    falling block, or if the falling block has reached the left edge of the
    grid. *)
let collision_left st =
  let left_col = optimum snd (<) (falling_block_coords st) in
  collision st (left_col - 1) 0 block_at_col_bound

(** [collision_right st] is true iff there is a block to the right the currently
    falling block, or if the falling block has reached the right edge of the
    grid. *)
let collision_right st =
  let right_col = optimum snd (>) (falling_block_coords st) in
  let num_cols = Array.length st.grid.(0) in
  collision st (right_col + 1) num_cols block_at_col_bound

(** [place_block st position tetromino value] takes [tetromino] and alters 
    [st]'s grid attribute. This is done by setting the rectangle that encloses 
    [tetromino] at grid [position] and setting the tetromino spots as [value].*)
let place_block st position tetromino value = 
  let mutate_grid (y, x) = 
    match position with
    | (r, c) -> st.grid.(r + x).(c + y) <- value in
  List.iter mutate_grid (Tetromino.get_comp tetromino);
  st.falling_block <- Some {
      block = tetromino;
      pos = position;
    }

(** [move st p] takes in the falling block in [st] and moves it one unit in 
    direction [dir]. *)
let move st dir =
  match st.falling_block with
  | None -> failwith "No block to move"
  | Some {block ; pos = (x, y)} ->
    begin
      let new_pos = 
        match dir with
        | Left -> (x, y - 1)
        | Right -> (x, y + 1)
        | Down -> (x + 1, y) in
      place_block st (x, y) block 0;
      place_block st new_pos block 1 
    end

let move_left st =
  (** Assuming that there's no collision TODO: Add collision check. *)
  move st Left

let move_right st =
  (** Assuming that there's no collision TODO: Add collision check. *)
  move st Right

let hold st =
  failwith "Unimplemented"

let fall st =
  (** Assuming that there's no collision TODO: Add collision check. *)
  move st Down

let update_score st =
  failwith "Unimplemented"

let grid_width st = Array.length st.grid.(0)

let grid_height st = Array.length st.grid

(** [get_top_left_pos tetromino] is the (row, column) entry in the grid that
    the top left corner of the rectangle that encloses [tetromino] be in. This
    entry is given by the formula:
    (0, (number of grid columns - block width - 1) / 2).
    For example, if there are 10 columns in the grid, the top left corner of the 
    L block will go in entry (0, (10 - 3 - 1) / 2) = (0, 3). *)
let get_top_left_pos tetromino st =
  (0, (grid_width st - Tetromino.get_width tetromino) / 2)

let spawn_tetromino tetromino st =
  (** Assuming that there's no collision while spawning yet. *)
  let top_left = get_top_left_pos tetromino st in 
  place_block st top_left tetromino 1

let drop st =
  while not (collision_under st) do
    move st Down
  done