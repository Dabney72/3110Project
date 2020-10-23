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
  composition = [(0, 1); (1, 1); (1, 0); (2, 0);];
}

let t_block = {
  composition = [(1, 0); (1, 1); (0, 1); (2, 1)];
}

let z_block = {
  composition = [(0, 0); (1, 0); (1, 1); (2, 1)];
}

let generate_list () = 
  failwith "Unimplemented"

let get_width tetromino = 
  failwith "Unimplemented"

let get_height tetromino = 
  failwith "Unimplemented"
