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
  let w1 = Random.float (-1.0) in
  let w2 = Random.float 1.0 in
  let w3 = Random.float (-1.0) in
  let w4 = Random.float (-1.0) in
  normalize [w1; w2; w3; w4]

let init_with_weights w1 w2 w3 w4 = [w1; w2; w3; w4]

(** [scale_vector vec c] scales each entry of [vec] by [c]. *)
let scale_vector vec c =
  List.map (fun x -> x *. c) vec

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

(** [move_score st s m] is [(score, m)], where [score] is the score that 
    strategy [s] gives [m] in state [st]. *)
let move_score st s m =
  m |> grid_after_move st |> move_outcome |> score s, m

(** [max_score current_max s] is [current_max] if [fst current_max]
    >= [fst s], and [s] otherwise. *)
let max_score current_max s =
  if fst current_max >= fst s then current_max else s

let move_next_piece s st =
  let next = get_falling_block st in
  let possible_moves = get_possible_moves next (grid_width st) in
  let scores = possible_moves |> List.map (move_score st s) in
  let move = List.fold_left max_score (List.hd scores) scores |> snd in 
  execute st move; 
  Display.draw_game_screen st; 
  Unix.sleepf 0.5; 
  drop st

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
    else loop (acc +. play_random_game s) (n + 1)
  in loop 0.0 0 /. float_of_int x

let to_list s = s