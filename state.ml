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
  mutable upcoming_blocks : Tetromino.tetromino_type list;
  mutable held_block : Tetromino.tetromino_type option;
  mutable score : int
}

let initialize () =
  {
    grid = Array.make_matrix 20 10 0;
    falling_block = None;
    upcoming_blocks = []; (*Tetromino.generate_list ();*)
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
      pos = position
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

let drop st =
  failwith "Unimplemented"

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
