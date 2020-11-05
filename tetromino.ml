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
  composition = [(0, 1); (1, 1); (2, 1); (3, 1)];
  width = 4;
}

let l_block = {
  composition = [(0, 0); (0, 1); (1, 1); (2, 1)];
  width = 3;
}

let j_block = {
  composition = [(2, 0); (0, 1); (1, 1); (2, 1)];
  width = 3;
}

let o_block = {
  composition = [(1, 0); (2, 0); (1, 1); (2, 1)];
  width = 4;
}

let s_block = {
  composition = [(0, 1); (1, 1); (1, 0); (2, 0)];
  width = 3;
}

let t_block = {
  composition = [(1, 0); (1, 1); (0, 1); (2, 1)];
  width = 3;
}

let z_block = {
  composition = [(0, 0); (1, 0); (1, 1); (2, 1)];
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
  let ct_list = [i_block;l_block;j_block;o_block;s_block;t_block;
                 z_block] in
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
  let w = tetromino.width in {
    composition = rotate_composition (w - 1) tetromino.composition;
    width = w
  }

let get_coord x y tetromino =
  if List.mem (x, y) tetromino.composition 
  then "||"
  else "  "

let create_str_grid coords tetromino =
  for x = 0 to Array.length coords - 1 do 
    for y = 0 to Array.length coords - 1 do
      coords.(y).(x) <- get_coord x y tetromino
    done
  done


let to_string tetromino =
  failwith "Unimplemented"
(* let coords = Array.make_matrix tetromino.width tetromino.width " " in
   create_str_grid coords tetromino;
   Array.fold_left (fun str arr -> ) *)
