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

let crossover s1 s2 =
  failwith "Unimplemented"

let mutate s =
  failwith "Unimplemented"

let score s o =
  failwith "Unimplemented"

(** [cmp_scores s st] compares [m1] and [m2] based on the score they get
    using strategy [s] in game state [st]. *)
let cmp_scores s st m1 m2 =
  let m1_outcome = grid_after_move st m1 |> move_outcome in
  let m2_outcome = grid_after_move st m2 |> move_outcome in
  match score s m1_outcome -. score s m2_outcome with
  | d when d > 0.0 -> 1
  | 0.0 -> 0
  | _ -> -1

let play_game s =
  let st = State.initialize () in
  while not (game_over st) do
    let next = List.hd (get_upcoming st) in
    let possible_moves = get_possible_moves next (grid_width st) in
    match List.sort (cmp_scores s st) possible_moves with
    | [] -> failwith "No possible moves detected" (* list of moves is empty *)
    | h :: t -> execute st h
  done; 
  float_of_int (get_lines_cleared st)