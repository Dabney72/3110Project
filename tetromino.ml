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
}

let i_block = {
  composition = [(0, 1); (1, 1); (2, 1); (3, 1)];
}

let l_block = {
  composition = [(0, 0); (0, 1); (1, 1); (2, 1)];
}

let j_block = {
  composition = [(2, 0); (0, 1); (1, 1); (2, 1)];
}

let o_block = {
  composition = [(1, 0); (2, 0); (1, 1); (2, 1)];
}

let s_block = {
  composition = [(0, 1); (1, 1); (1, 0); (2, 0)];
}

let t_block = {
  composition = [(1, 0); (1, 1); (0, 1); (2, 1)];
}

let z_block = {
  composition = [(0, 0); (1, 0); (1, 1); (2, 1)];
}

let create_tetromino = function
  | I_block -> i_block
  | L_block -> l_block
  | J_block -> j_block
  | O_block -> o_block
  | S_block -> s_block
  | T_block -> t_block
  | Z_block -> z_block

let generate_list () = 
  let ct_list = [i_block;l_block;j_block;o_block;s_block;t_block;
                 z_block] in
  let start_lst = List.map (fun c -> (Random.bits (),c)) ct_list
  in List.sort compare start_lst |> List.map snd 

let get_width tetromino = 
  failwith "Unimplemented"

(** [rotate_composition offset comp] is a new list of coordinates obtained
    after rotating each coordinate in [comp] 90 degrees clockwise. *)
let rec rotate_composition offset = function
  | [] -> []
  | (x, y) :: t -> (-y + offset, x) :: rotate_composition offset t

let rotate tetromino = 
  let offset = get_width tetromino - 1 in {
    composition = rotate_composition offset tetromino.composition
  }
