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
  left_col - 1 = 0
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
let place_block st position tetromino value = 
  let mutate_grid (y, x) = 
    match position with
    | (r, c) -> st.grid.(r + x).(c + y) <- value in
  List.iter mutate_grid (Tetromino.get_comp tetromino);
  st.falling_block <- Some {
      block = tetromino;
      pos = position;
    }

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

let spawn_tetromino tetromino st =
  let (r, c) as top_left = get_top_left tetromino st in 
  let trunc = truncate tetromino in
  if can_spawn tetromino top_left st
  then place_block st top_left tetromino 1
  else if can_spawn trunc (r - 1, c) st
  then begin
    place_block st (r - 1, c) trunc 1;
    st.game_over <- true
  end
  else st.game_over <- true

let spawn_next st =
  match st.upcoming_blocks with
  | [] -> failwith "Error: no upcoming blocks found"
  | h :: t -> 
    let next_block = init_tetromino h in
    spawn_tetromino next_block st;
    if List.length t <= 3
    then st.upcoming_blocks <- t @ generate_list ()
    else st.upcoming_blocks <- t 

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

let rotate st =
  let falling = get_falling st in
  place_block st falling.pos falling.block 0;
  place_block st falling.pos (rotate falling.block) 1

let drop ?auto_respawn:(ar = true) st =
  while not (collision_under st) do
    move st Down
  done;
  if ar then spawn_next st

let fall ?auto_respawn:(ar = true) st =
  if not (collision_under st)
  then move st Down
  else if ar then spawn_next st

let hold st =
  failwith "Unimplemented"

let update_score st =
  failwith "Unimplemented"

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
