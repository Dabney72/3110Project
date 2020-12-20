open Tetromino

let pp_array pp_elt arr = 
  let build_string acc x = 
    acc ^ "; " ^ pp_elt x in
  let s = Array.fold_left build_string "" arr in 
  "[|" ^ String.sub s 2 (String.length s - 2) ^ "|]"

let pp_int_matrix m = 
  let aux acc row = 
    acc ^ "; \n" ^ pp_array string_of_int row  in
  let s = Array.fold_left aux "" m in 
  "[|" ^ String.sub s 2 (String.length s - 2) ^ "\n|]"

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"


(** [get_coord x y tetromino] is "X" if it corresponds to a block on
    [tetromino], and "*" if it does not. *)
let get_coord x y tetromino =
  if List.mem (x, y) (get_comp tetromino)
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

let pp_tetromino tetromino =
  let width = get_width tetromino in
  let coords = Array.make_matrix width width " " in
  create_str_grid coords tetromino;
  "\n\n" 
  ^ Array.fold_left (fun acc arr -> acc ^ row_to_string arr) "" coords ^
  "\n"