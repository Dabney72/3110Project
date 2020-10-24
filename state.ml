type falling = {
  block : Tetromino.t;
  pos : (int * int);
}

type t = {
  grid : int array array;
  falling_block : falling option;
  upcoming_blocks : Tetromino.t list;
  held_block : Tetromino.t option;
  score : int
}

let initialize () =
  {
    grid = Array.make_matrix 10 20 0;
    falling_block = None;
    upcoming_blocks = Tetromino.generate_list ();
    held_block = None;
    score = 0;
  }

let get_grid t =
  t.grid

let rotate st =
  failwith "Unimplemented"

let move_left st =
  failwith "Unimplemented"

let move_right st =
  failwith "Unimplemented"

let drop st =
  failwith "Unimplemented"

let hold st =
  failwith "Unimplemented"

let fall st =
  failwith "Unimplemented"

let update_score st =
  failwith "Unimplemented"

let grid_width st =
  failwith "Unimplemented"

let grid_height st =
  failwith "Unimplemented"

(** [get_top_left_pos tetromino] is the (row, column) entry in the grid that
    the top left corner of the rectangle that encloses [tetromino] be in. This
    entry is given by the formula:
    (0, (number of grid columns - block width - 1) / 2).
    For example, if there are 10 columns in the grid, the top left corner of the 
    L block will go in entry (0, (10 - 3 - 1) / 2) = (0, 3). *)
let get_top_left_pos st tetromino =
  (0, (grid_width st - Tetromino.get_width tetromino - 1) / 2)

let spawn_tetromino st tetromino =
  match get_top_left_pos st tetromino with
  | (r, c) -> assert false (* iterate / pattern match against tetromino's 
                              composition list, and for every coordinate (x,y), set the entry in the
                              matrix given by (r + x, c + y) to 1 (later, we will have to also check to
                              make sure the matrix entry isn't already 1). *)

let get_upcoming_blocks st = 
  failwith "Unimplemented"