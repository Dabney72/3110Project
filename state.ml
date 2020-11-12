open Tetromino

type direction = 
  | Left
  | Right
  | Down

type falling = {
  block_type : Tetromino.tetromino_type;
  mutable block : Tetromino.t;
  mutable pos : (int * int);
}

type t = {
  mutable grid : int array array;
  mutable falling_block : falling option;
  mutable upcoming_blocks : tetromino_type list;
  mutable held_block : tetromino_type option;
  mutable score : int;
  mutable game_over : bool;
}

(** [get_falling st] is the current block that is falling.
    Raises: Failure if no block is currently falling. *)
let get_falling st = 
  match st.falling_block with
  | None -> failwith "No falling block"
  | Some f -> f

let get_grid st =
  st.grid

let get_score t =
  t.score

let get_hold t =
  t.held_block

let get_upcoming t =
  t.upcoming_blocks

let game_over t =
  t.game_over

let grid_width st = Array.length st.grid.(0)

let grid_height st = Array.length st.grid

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

(** [block_coords tetromino pos st] is a list of (row, column) coordinates for the 
    tetromino [tetr] at position [pos] in state [st]. *)
let block_coords tetromino pos st =
  let rec help comp =
    match comp with
    | [] -> []
    | (y, x) :: t -> 
      match pos with
      | (r, c) -> (r + x, c + y) :: help t
  in help (get_comp tetromino)

(** [blocks_surrounding st offset_r offset_c comp] is whether there is a block
    next to any of the coordinates in [comp]. [offset_r] and [offset_c]
    determine where to check, e.g. it will check whether there is a block at
    (r + offset_r, c + offset_c) for any (r, c) in [comp]. 
    Requires: (r + offset_r, c + offset_c) is not out of the grid bounds for 
    any (r, c) in [comp]. *)
let blocks_surrounding st offset_r offset_c comp =
  let rec loop st = function
    | [] -> false
    | (r', c') :: t ->
      let r = r' + offset_r in
      let c = c' + offset_c in 
      not (List.mem (r, c) comp) (* Make sure block can't collide with itself *)
      && st.grid.(r).(c) = 1 
      || loop st t
  in loop st comp

(** [collision_under st] is true iff there is a block under the currently
    falling block, or if the falling block has reached the bottom of the
    grid. *)
let collision_under st =
  let falling = get_falling st in
  let coords = block_coords falling.block falling.pos st in
  let bottom_row = optimum fst (>) coords in
  bottom_row + 1 = Array.length st.grid
  ||
  blocks_surrounding st 1 0 coords

(** [collision_left st] is true iff there is a block to the left the currently
    falling block, or if the falling block has reached the left edge of the
    grid. *)
let collision_left st =
  let falling = get_falling st in
  let coords = block_coords falling.block falling.pos st in
  let left_col = optimum snd (<) coords in
  left_col = 0
  ||
  blocks_surrounding st 0 (-1) coords

(** [collision_right st] is true iff there is a block to the right the currently
    falling block, or if the falling block has reached the right edge of the
    grid. *)
let collision_right st =
  let falling = get_falling st in
  let coords = block_coords falling.block falling.pos st in
  let right_col = optimum snd (>) coords in
  right_col + 1 = Array.length st.grid.(0)
  ||
  blocks_surrounding st 0 1 coords

(** [place_block st position tetromino value] takes [tetromino] and alters 
    [st]'s grid attribute. This is done by setting the rectangle that encloses 
    [tetromino] at grid [position] and setting the tetromino spots as [value].*)
let place_block st falling value =
  let mutate_grid (y, x) = 
    match falling.pos with
    | (r, c) -> st.grid.(r + x).(c + y) <- value in
  List.iter mutate_grid (Tetromino.get_comp falling.block);
  st.falling_block <- Some falling

(** [get_top_left tetromino] is the (row, column) entry in the grid that
    the top left corner of the rectangle that encloses [tetromino] be in. This
    entry is given by the formula:
    (0, (number of grid columns - block width) / 2). *)
let get_top_left tetromino st =
  (0, (grid_width st - Tetromino.get_width tetromino) / 2)

(** [can_spawn tetromino pos st] is whether [tetromino] can be spawned at 
    position [pos] in state [st]. *)
let can_spawn tetromino pos st =
  let rec help = function
    | [] -> true
    | (r, c) :: t -> st.grid.(r).(c) == 0 && help t
  in block_coords tetromino pos st |> help

(** [truncate tetromino] is [tetromino] with its top row cut off. Used for
    safely filling the grid with a final block when the game is over. *)
let truncate tetromino =
  let crds = tetromino
             |> get_comp
             |> List.filter (fun (_, y) -> y >= 1) in
  create_tetromino crds (get_width tetromino)

let spawn_tetromino tetromino_type st =
  let tetromino = init_tetromino tetromino_type in
  let (r, c) as top_left = get_top_left tetromino st in 
  let falling = {
    block_type = tetromino_type;
    block = tetromino;
    pos = top_left;
  } in
  let trunc = truncate tetromino in
  let trunc_falling = { falling with block = trunc; pos = (r - 1, c) } in
  if can_spawn tetromino top_left st
  then place_block st falling 1
  else if can_spawn trunc (r - 1, c) st
  then begin
    place_block st trunc_falling 1;
    st.game_over <- true
  end
  else st.game_over <- true

let spawn_next st =
  match st.upcoming_blocks with
  | [] -> failwith "Error: no upcoming blocks found"
  | h :: t -> 
    spawn_tetromino h st;
    if List.length t <= 3
    then st.upcoming_blocks <- t @ generate_list ()
    else st.upcoming_blocks <- t 

(** [increment_score st] updates the player's score according to the number of
    rows they have completely filled and returns a list of the indexes of the 
    rows that were accounted for while adding score. *)
let increment_score st =
  let rows = ref [] in
  let get_points = function
    | 1 -> 40
    | 2 -> 100
    | 3 -> 300
    | 4 -> 1200 
    | _ -> 0 in 
  let consec = ref 0 in
  let points = ref 0 in
  let inc_score index row = 
    if Array.for_all (fun x -> x = 1) row 
    then begin 
      consec := !consec + 1; 
      rows := index :: !rows 
    end
    else begin
      points := !points + get_points !consec; 
      consec := 0 
    end in
  Array.iteri inc_score st.grid;
  points := !points + get_points !consec;
  st.score <- st.score + !points;
  !rows

(** [update_score st] checks to see if any rows are completely filled and 
    updates the player's score depending on how many rows are filled. A single
    row is 40 points, two rows is 100 points, three rows is 300 points, and 
    four rows is 1200 points. *)
let update_score st =
  let filled_rows = increment_score st in
  let unfilled_rows = ref [||] in
  let drop_rows index row = 
    if not (List.mem index filled_rows)
    then unfilled_rows := Array.append !unfilled_rows [|row|] in
  Array.iteri drop_rows st.grid;
  let new_height = List.length filled_rows in
  let new_rows = Array.make_matrix new_height (grid_width st) 0 in 
  let updated_grid = Array.append new_rows !unfilled_rows in
  st.grid <- updated_grid

(** [move st p] takes in the falling block in [st] and moves it one unit in 
    direction [dir]. *)
let move st dir =
  match st.falling_block with
  | None -> failwith "No block to move"
  | Some ({pos = (x, y)} as falling) ->
    begin
      let new_pos = 
        match dir with
        | Left -> (x, y - 1)
        | Right -> (x, y + 1)
        | Down -> (x + 1, y) in
      place_block st falling 0;
      place_block st { falling with pos = new_pos } 1 
    end

let move_left st =
  if not (collision_left st) then move st Left

let move_right st =
  if not (collision_right st) then move st Right

(** [valid_position st] takes in the state [st] and returns the block that
    is falling. 
    [Failure "No block falling"] if st.falling_block is None  *)
let valid_position st old_pos r c =
  if r < 0 || c < 0 then false 
  else if r >= grid_height st || c >= grid_width st then false
  else st.grid.(r).(c) = 0  || List.mem(r, c) old_pos 

let rotate st =
  let falling = get_falling st in
  let rotate_comp = get_comp (rotate falling.block) in
  let add_pos acc (y, x) = (fst falling.pos + x, snd falling.pos + y) :: acc in
  let falling_positions = List.fold_left add_pos [] (get_comp falling.block) in 
  let valid_placement (y, x) =
    match st.falling_block with 
    | None -> failwith "No block falling"
    | Some {block; pos = (r, c)} -> 
      valid_position st falling_positions (r + x) (c + y) in
  if List.for_all valid_placement rotate_comp 
  then begin
    place_block st falling 0;
    place_block st {falling with block = rotate falling.block} 1
  end

let drop ?auto_respawn:(auto = true) st =
  while not (collision_under st) do
    move st Down
  done;
  update_score st;
  if auto then spawn_next st

let fall ?auto_respawn:(auto = true) st =
  if not (collision_under st)
  then move st Down
  else begin 
    update_score st; 
    if auto then spawn_next st 
  end

let hold st =
  let fall_block = get_falling st in
  match st.held_block with
  | None -> begin
      st.held_block <- Some fall_block.block_type;
      place_block st fall_block 0;
      spawn_next st
    end
  | Some c -> begin
      st.held_block <- Some fall_block.block_type;
      place_block st fall_block 0;
      spawn_tetromino c st
    end

let initialize ?auto_spawn:(auto = true) () =
  let st = {
    grid = Array.make_matrix 20 10 0;
    falling_block = None;
    upcoming_blocks = generate_list ();
    held_block = None;
    score = 0;
    game_over = false;
  } in
  if auto then spawn_next st;
  st
