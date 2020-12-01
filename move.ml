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

let grid_after_move st m =
  failwith "Unimplemented"

let rotate_n n st =
  for i = 1 to n do st |> State.rotate_cw done

let left_n n st =
  for i = 1 to n do st |> State.move_left done

let right_n n st =
  for i = 1 to n do st |> State.move_right done

let execute st m =
  rotate_n m.rotations st;
  left_n m.moves_left st;
  right_n m.moves_right st

(** [aggregate_height grid] is the sum of the height of each line in [grid]. *)
let aggregate_height grid = 
  failwith "Unimplemented"

(** [complete_lines grid] is the number of complete lines in [grid]. *)
let complete_lines grid =
  failwith "Unimplemented"

(** [holes grid] is the number of holes in [grid]. A hole is defined by an
    empty square with a full square directly on top of it. *)
let holes grid =
  failwith "Unimplemented"

(** [bumpiness grid] is the variation of column heights in [grid], calculated
    by summing the absolute value of the difference in height of adjacent
    columns. *)
let bumpiness grid =
  failwith "Unimplemented"

let move_outcome m proj_grid = [
  aggregate_height proj_grid;
  complete_lines proj_grid;
  holes proj_grid;
  bumpiness proj_grid
] |> List.map float_of_int

let get_possible_moves tt grid_width =
  failwith "Unimplemented"