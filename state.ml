open Tetromino

module ShadowMap = Map.Make(Int)

type direction = 
  | Left
  | Right
  | Down

type falling = {
  block_type : tetromino_type;
  mutable block : Tetromino.t;
  mutable pos : (int * int);
}

type t = {
  mutable grid : tetromino_type option array array;
  mutable falling_block : falling option;
  mutable shadow_block : (int * int) list;
  mutable upcoming_blocks : tetromino_type list;
  mutable held_block : tetromino_type option;
  mutable score : int;
  mutable level : int;
  mutable lines_cleared : int;
  mutable game_over : bool;
  mutable use_hold : bool;
  mutable combo_multiplier : int;
}

(** [get_falling st] is the current block that is falling.
    Raises: Failure if no block is currently falling. *)
let get_falling st = 
  match st.falling_block with
  | None -> failwith "No falling block"
  | Some f -> f

let get_falling_block st =
  (get_falling st).block_type

let get_grid st =
  st.grid

let get_score t =
  t.score

let get_hold t =
  t.held_block

let get_upcoming t =
  t.upcoming_blocks

let get_level t =
  t.level

let get_lines_cleared t =
  t.lines_cleared

let get_combo_multi t =
  t.combo_multiplier

let game_over t =
  t.game_over

let use_hold t =
  t.use_hold

let grid_width st =
  Array.length st.grid.(0)

let grid_height st =
  Array.length st.grid

let convert_opt = function
  | None 
  | Some Shadow -> 0
  | _ -> 1

let copy_row acc arr = 
  Array.append acc [|Array.copy arr|]

let copy_grid st =
  Array.fold_left copy_row [||] st.grid

let copy_grid_int st =
  copy_grid st |> Array.map (Array.map convert_opt)

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

(** [block_coords tetromino pos st] is a list of (row, column) coordinates for 
    the tetromino [tetr] at position [pos] in state [st]. *)
let block_coords tetromino pos st =
  let rec block_coords_aux comp =
    match comp with
    | [] -> []
    | (y, x) :: t -> 
      match pos with
      | (r, c) -> (r + x, c + y) :: block_coords_aux t
  in block_coords_aux (get_comp tetromino)

(** [blocks_surrounding st offset_r offset_c comp] is whether there is a block
    next to any of the coordinates in [comp]. [offset_r] and [offset_c]
    determine where to check, e.g. it will check whether there is a block at
    (r + offset_r, c + offset_c) for any (r, c) in [comp]. 
    Requires: (r + offset_r, c + offset_c) is not out of the grid bounds for 
    any (r, c) in [comp]. *)
let blocks_surrounding st offset_r offset_c comp =
  let valid_row_col r c = match st.grid.(r).(c) with 
    | Some Shadow -> false
    | None -> false
    | _-> true in
  let rec loop st = function
    | [] -> false
    | (r', c') :: t ->
      let r = r' + offset_r in
      let c = c' + offset_c in 
      not (List.mem (r, c) comp) && valid_row_col r c || loop st t
  in loop st comp

(** [collision_under st] is true iff there is a block under the currently
    falling block, or if the falling block has reached the bottom of the
    grid. *)
let collision_under st =
  let falling = get_falling st in
  let coords = block_coords falling.block falling.pos st in
  let bottom_row = optimum fst (>) coords in
  bottom_row + 1 = Array.length st.grid || blocks_surrounding st 1 0 coords

(** [collision_left st] is true iff there is a block to the left the currently
    falling block, or if the falling block has reached the left edge of the
    grid. *)
let collision_left st =
  let falling = get_falling st in
  let coords = block_coords falling.block falling.pos st in
  let left_col = optimum snd (<) coords in
  left_col = 0 || blocks_surrounding st 0 (-1) coords

(** [collision_right st] is true iff there is a block to the right the currently
    falling block, or if the falling block has reached the right edge of the
    grid. *)
let collision_right st =
  let falling = get_falling st in
  let coords = block_coords falling.block falling.pos st in
  let right_col = optimum snd (>) coords in
  right_col + 1 = Array.length st.grid.(0) || blocks_surrounding st 0 1 coords

(** [place_block st position tetromino value] takes [tetromino] and alters 
    [st]'s grid attribute. This is done by setting the rectangle that encloses 
    [tetromino] at grid [position] and setting the tetromino spots as [value].*)
let place_block st falling value =
  let mutate_grid (y, x) = 
    match falling.pos with
    | (r, c) -> st.grid.(r + x).(c + y) <- value in
  List.iter mutate_grid (get_comp falling.block);
  st.falling_block <- Some falling

(** [get_top_left tetromino] is the (row, column) entry in the grid that
    the top left corner of the rectangle that encloses [tetromino] be in. This
    entry is given by the formula:
    (0, (number of grid columns - block width) / 2). *)
let get_top_left tetromino st =
  (0, (grid_width st - get_width tetromino) / 2)

(** [maximum_drop st height lst] is [lst] but every (row, col) pair is projected
    down by a constant that is the minimum of the distances between each 
    pair and the first placed block directly below that (row, col). *)
let maximum_drop st height lst = 
  let update_lowest lowest (r, c) = 
    let r' = ref r in
    while (List.mem (!r', c) lst) do 
      r' := !r' + 1
    done; 
    while (!r' + 1) < height && Option.is_none(st.grid.(!r' + 1).(c)) do
      r' := !r' + 1
    done;
    if !r' = 20 then 0 else min (!r' - r) lowest in
  let change = List.fold_left update_lowest max_int lst in
  let add (r, c) = (r + change, c) in 
  List.map add lst

(** [shadow_pos st falling] is a list of (row, col) where the shadow for the
    current falling block should be placed. *)
let shadow_pos st falling = 
  let height = grid_height st in
  let falling_pos (y, x) = 
    match falling.pos with
    | (r, c) -> (r + x), (c + y) in
  let valid_spot (r, c) = Option.is_none st.grid.(r).(c) in 
  get_comp falling.block  
  |> List.map falling_pos
  |> maximum_drop st height
  |> List.filter valid_spot 

(** [create_shadow st] creates a shadow for the falling block in order 
    to predict where that block will drop. *)
let create_shadow st =
  match st.falling_block with
  | Some falling -> 
    let place_shadow (r, c) = st.grid.(r).(c) <- Some Shadow in
    let location = shadow_pos st falling in
    List.iter place_shadow location; st.shadow_block <- location 
  | None -> st.shadow_block <- []

let delete_shadow st = 
  let remove (r, c) = 
    if st.grid.(r).(c) = Some Shadow then st.grid.(r).(c) <- None in
  List.iter remove st.shadow_block

(** [can_spawn tetromino pos st] is whether [tetromino] can be spawned at 
    position [pos] in state [st]. *)
let can_spawn tetromino pos st =
  let rec help = function
    | [] -> true
    | (r, c) :: t -> Option.is_none st.grid.(r).(c) && help t
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
  then place_block st falling (Some tetromino_type)
  else if can_spawn trunc (r - 1, c) st
  then begin
    place_block st trunc_falling (Some tetromino_type);
    st.game_over <- true
  end
  else st.game_over <- true

let spawn_next st =
  match st.upcoming_blocks with
  | [] -> failwith "Error: no upcoming blocks found"
  | h :: t -> 
    delete_shadow st;
    spawn_tetromino h st;
    if not (collision_under st) then create_shadow st;
    if List.length t <= 3
    then st.upcoming_blocks <- t @ generate_list ()
    else st.upcoming_blocks <- t 

let increment_lines_cleared st ln =
  st.lines_cleared <- st.lines_cleared + ln;
  if st.lines_cleared >= 10 * st.level && st.level < 10
  then st.level <- st.level + 1

let update_grid grid st = 
  st.grid <- grid;
  st

(** [get_points st lines] updates state by adding [lines] to the current number 
    of lines cleared to the state and is the score corresponding to [lines]. *)
let get_points st lines = match lines with
  | 1 -> increment_lines_cleared st 1; 40
  | 2 -> increment_lines_cleared st 2; 100
  | 3 -> increment_lines_cleared st 3; 300
  | 4 -> increment_lines_cleared st 4; 1200 
  | _ -> 0

(** [increment_score st] updates the player's score according to the number of
    rows they have completely filled and returns a list of the indexes of the 
    rows that were accounted for while adding score. *)
let increment_score st =
  let rows = ref [] in
  let consec = ref 0 in
  let points = ref 0 in
  let inc_score index row = 
    if Array.for_all Option.is_some row 
    then begin 
      consec := !consec + 1; 
      rows := index :: !rows 
    end
    else begin
      points := !points + get_points st !consec; 
      consec := 0 
    end in
  Array.iteri inc_score st.grid;
  points := !points + get_points st !consec;
  st.score <- st.score + (st.combo_multiplier * !points);
  st.combo_multiplier <- if !points > 0 then st.combo_multiplier + 1 else 1;
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
  let new_rows = Array.make_matrix new_height (grid_width st) None in 
  let updated_grid = Array.append new_rows !unfilled_rows in
  st.grid <- updated_grid

(** [move st p] takes in the falling block in [st] and moves it one unit in 
    direction [dir]. *)
let move st dir =
  delete_shadow st;
  match st.falling_block with
  | None -> failwith "No block to move"
  | Some ({block_type; pos = (x, y)} as falling) ->
    begin
      let new_pos = 
        match dir with
        | Left -> (x, y - 1)
        | Right -> (x, y + 1)
        | Down -> (x + 1, y) in
      place_block st falling None;
      place_block st { falling with pos = new_pos } (Some block_type);
      create_shadow st
    end

let move_left st =
  if not (collision_left st) then move st Left

let move_right st =
  if not (collision_right st) then move st Right

(** [valid_position st positions r c] returns true if [r] and [c] are bound 
    between the grid and position row = r, column = c in the grid is either 
    empty or one of the original [positions]. *)
let valid_position st positions r c =
  if r < 0 || c < 0 then false 
  else if r >= grid_height st || c >= grid_width st then false
  else Option.is_none st.grid.(r).(c) || List.mem (r, c) positions 

let rotate dir st =
  let falling = get_falling st in
  let rotate_comp = get_comp (dir falling.block) in
  let add_pos acc (y, x) = (fst falling.pos + x, snd falling.pos + y) :: acc in
  let falling_positions = List.fold_left add_pos [] (get_comp falling.block) in 
  let valid_placement (y, x) =
    match st.falling_block with 
    | None -> failwith "No block falling"
    | Some {block; pos = (r, c)} -> 
      valid_position st falling_positions (r + x) (c + y) in
  if List.for_all valid_placement rotate_comp 
  then begin
    delete_shadow st;
    place_block st falling None;
    place_block st {falling with block = dir falling.block} 
      (Some falling.block_type);
    create_shadow st
  end

let rotate_cw =
  rotate rotate_cw

let rotate_ccw =
  rotate rotate_ccw

let drop ?line_clears:(lc=true) ?auto_respawn:(auto=true) st =
  while not (collision_under st) do
    move st Down
  done;
  if lc then update_score st;
  st.use_hold <-true;
  if auto then spawn_next st

let fall ?auto_respawn:(auto = true) st =
  if not (collision_under st)
  then begin 
    move st Down; 
    if (collision_under st)
    then begin delete_shadow st end
  end
  else begin
    update_score st; 
    st.use_hold <-true;
    if auto then spawn_next st 
  end

let hold st =
  if use_hold st then begin
    let fall_block = get_falling st in
    match st.held_block with
    | None -> begin
        st.held_block <- Some fall_block.block_type;
        place_block st fall_block None;
        st.use_hold <- false;
        spawn_next st
      end
    | Some c -> begin
        st.held_block <- Some fall_block.block_type;
        place_block st fall_block None;
        st.use_hold <- false;
        spawn_tetromino c st
      end
  end
  else ()

let initialize ?first_block:(first = None) ?auto_spawn:(auto = true )() =
  let st = {
    grid = Array.make_matrix 20 10 None;
    falling_block = None;
    shadow_block = [];
    upcoming_blocks = (match first with 
        | Some tetr -> (tetr :: generate_list ())
        | None -> generate_list ());
    held_block = None;
    score = 0;
    level = 1;
    lines_cleared = 0;
    game_over = false;
    use_hold = true;
    combo_multiplier = 1;
  } in
  if auto then spawn_next st;
  st

let copy_grid_and_falling st = 
  {(initialize ()) with grid = copy_grid st;
                        falling_block = Some (get_falling st)}
