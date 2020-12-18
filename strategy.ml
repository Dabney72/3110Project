open State
open Move

type t = float list

let normalize lst =
  let norm =
    lst
    |> List.fold_left (fun acc x -> acc +. x ** 2.0) 0.0
    |> sqrt
  in List.map (fun x -> x /. norm) lst

let initialize () =
  let a = Random.float (-1.0) in
  let b = Random.float 1.0 in
  let c = Random.float (-1.0) in
  let d = Random.float (-1.0) in
  normalize [a; b; c; d]

let init_with_weights a b c d = 
  [a; b; c; d]

(** [scale_vector v c] scales each entry of [v] by [c]. *)
let scale_vector v c =
  List.map (fun x -> x *. c) v

let crossover s1 s2 f1 f2 =
  let v1, v2 = scale_vector s1 f1, scale_vector s2 f2 in
  List.map2 ( +. ) v1 v2

(** [add_to_nth offset i lst] is [lst] with [offset] added to its ith element.
    if i is not in bounds, then it will just return [lst]. *)
let add_to_nth offset i lst =
  let rec loop n acc = function
    | [] -> List.rev acc
    | h :: t -> begin 
        let acc' = (if n = i then h +. offset else h) :: acc in
        loop (n + 1) acc' t
      end 
  in loop 0 [] lst

let mutate s =
  let offset = Random.float 4.0 -. 2.0 in
  let i = Random.int (List.length s) in
  add_to_nth offset i s |> normalize

let score s o =
  List.fold_right2 (fun w p acc -> acc +. w *. p) s o 0.0

(** [cmp_scores s st] compares [m1] and [m2] based on the score they get
    using strategy [s] in game state [st]. *)
let cmp_scores s st m1 m2 =
  let m1_outcome = grid_after_move st m1 |> move_outcome in
  let m2_outcome = grid_after_move st m2 |> move_outcome in
  score s m2_outcome -. score s m1_outcome |> int_of_float

let move_next_piece s st =
  let next = get_falling_block st in
  let possible_moves = get_possible_moves next (grid_width st) in
  let sorted = List.sort (cmp_scores s st) possible_moves in
  match sorted with
  | [] -> failwith "No possible moves detected" (* list of moves is empty *)
  | h :: _ -> execute st h; drop st

let play_random_game s =
  let st = State.initialize () in
  while not (game_over st) && get_lines_cleared st < 100 do
    move_next_piece s st
  done; 
  float_of_int (get_lines_cleared st)

let train s x =
  let rec loop acc n = 
    if n = x
    then acc 
    else loop (play_random_game s :: acc) (n + 1)
  in 
  let outcome = loop [] 0 in
  List.fold_left ( +. ) 0. outcome /. float_of_int (List.length outcome)

let to_list s = s