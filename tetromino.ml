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

(** [cw offset comp] is a new list of coordinates obtained
    after rotating each coordinate in [comp] 90 degrees clockwise. *)
let rec cw offset = function
  | [] -> []
  | (x, y) :: t -> (-y + offset, x) :: cw offset t

(** [ccw offset comp] is a new list of coordinates obtained
    after rotating each coordinate in [comp] 90 degrees counterclockwise. *)
let rec ccw offset = function
  | [] -> []
  | (x, y) :: t -> (y, offset - x) :: ccw offset t

(** [rotate dir tetromino] is [tetromino] after [dir] rotates its coordinates.
    It will shift the tetromino after rotation in order to keep it within the
    same boundary. *)
let rotate dir tetromino =
  let comp = tetromino.composition in
  let w = tetromino.width in 
  if comp = o_block.composition 
  && w = o_block.width
  then tetromino
  else {
    tetromino with composition = dir (w - 1) comp;
  }

let rotate_cw =
  rotate cw

let rotate_ccw =
  rotate ccw

let get_name = function
  | I_block -> "I block"
  | L_block -> "L block"
  | J_block -> "J block"
  | O_block -> "O block"
  | S_block -> "S block"
  | T_block -> "T block"
  | Z_block -> "Z block"
