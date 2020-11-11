type tetromino_type =
  | I_block
  | L_block
  | J_block
  | O_block
  | S_block
  | T_block
  | Z_block

type t = {
  composition : (int * int) list;
  width : int
}

let i_block = {
  composition = [(0,1); (1,1); (2,1); (3,1)];
  width = 4;
}

let j_block = {
  composition = [(0,0); (0,1); (1,1); (2,1)];
  width = 3;
}

let l_block = {
  composition = [(2,0); (0,1); (1,1); (2,1)];
  width = 3;
}

let o_block = {
  composition = [(1,0); (2,0); (1,1); (2,1)];
  width = 4;
}

let s_block = {
  composition = [(0,1); (1,1); (1,0); (2,0)];
  width = 3;
}

let t_block = {
  composition = [(1,0); (1,1); (0,1); (2,1)];
  width = 3;
}

let z_block = {
  composition = [(0,0); (1,0); (1,1); (2,1)];
  width = 3;
}

let init_tetromino = function
  | I_block -> i_block
  | L_block -> l_block
  | J_block -> j_block
  | O_block -> o_block
  | S_block -> s_block
  | T_block -> t_block
  | Z_block -> z_block

let create_tetromino comp w = {
  composition = comp;
  width = w
}

let generate_list () = 
  let ct_list = [I_block;L_block;J_block;O_block;S_block;T_block;
                 Z_block] in
  let start_lst = List.map (fun c -> (Random.bits (),c)) ct_list
  in List.sort compare start_lst |> List.map snd 

let get_width tetromino = 
  tetromino.width

let get_comp tetromino =
  tetromino.composition

(** [rotate_composition offset comp] is a new list of coordinates obtained
    after rotating each coordinate in [comp] 90 degrees clockwise. *)
let rec rotate_composition offset = function
  | [] -> []
  | (x, y) :: t -> (-y + offset, x) :: rotate_composition offset t

let rotate tetromino =
  let comp = tetromino.composition in
  let w = tetromino.width in 
  if comp = o_block.composition 
  && w = o_block.width
  then tetromino
  else {
    tetromino with composition = rotate_composition (w - 1) comp;
  }

(** [get_coord x y tetromino] is "X" if it corresponds to a block on
    [tetromino], and "*" if it does not. *)
let get_coord x y tetromino =
  if List.mem (x, y) tetromino.composition 
  then "X"
  else "*"

(** [create_str_grid coords tetromino] is an array representing the coordinate
    system of [tetromino]. The array fills entries corresponding with blocks
    with "X", and entries corresponding to an empty space with "*". *)
let create_str_grid coords tetromino =
  for x = 0 to Array.length coords - 1 do 
    for y = 0 to Array.length coords - 1 do
      coords.(y).(x) <- get_coord x y tetromino
    done
  done

(** [row_to_string row] concatenates all of the strings in [row]. *)
let row_to_string row =
  Array.fold_left (^) "" row ^ "\n"

let to_string tetromino =
  let coords = Array.make_matrix tetromino.width tetromino.width " " in
  create_str_grid coords tetromino;
  "\n\n" 
  ^ Array.fold_left (fun acc arr -> acc ^ row_to_string arr) "" coords ^
  "\n"

let get_name = function
  | I_block -> "I block"
  | L_block -> "L block"
  | J_block -> "J block"
  | O_block -> "O block"
  | S_block -> "S block"
  | T_block -> "T block"
  | Z_block -> "Z block"
