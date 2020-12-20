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

let move_next_piece ?slow:(slow=true) s st =
  let next = get_falling_block st in
  let possible_moves = get_possible_moves next (grid_width st) in
  let scores = possible_moves |> List.map (move_score st s) in
  let move = List.fold_left max_score (List.hd scores) scores |> snd in 
  execute st move; 
  if slow then (Display.draw_game_screen st; Unix.sleepf 0.5);
  drop st

(** [fitness d threshold n] is the fitness score, which is 0.0 if 
    [n] < [threshold], and [n] otherwise.
    If [p] is true, it additionally prints the result of the game
    (i.e. whether or not the threshold was reached). *)
let fitness p threshold = function
  | l1 when l1 < float_of_int threshold -> 
    if p then print_endline "Strategy did not achieve threshold"; 0.0
  | l2 -> 
    if p then print_endline "Strategy achieved 100 cleared lines"; l2
[@@coverage off] (* Coverage is off because this is a helper function for
                    [train]. *)

let play_random_game ?debug:(d=false) s =
  let st = State.initialize () in
  let threshold = 1000 in
  while not (game_over st) && get_lines_cleared st < threshold do
    if d
    then move_next_piece ~slow:false s st
    else move_next_piece s st
  done;
  fitness d threshold (float_of_int (get_lines_cleared st))
[@@coverage off] (* Coverage is off because this is a helper function for
                    [train]. *)

let train ?debug:(d=false) s x =
  let rec loop acc n = 
    if n = x
    then acc 
    else loop (acc +. play_random_game ~debug: d s) (n + 1)
  in loop 0.0 0 /. float_of_int x
[@@coverage off] (* Coverage is off because this cannot be automatically
                    tested with OUnit, as there are no properties we can
                    predict. To test this function, we use strategies.ml
                    to see if the strategies are getting better as they are
                    trained. *)

let to_list s = s
