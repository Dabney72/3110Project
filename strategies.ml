open Strategy
open Printers

module IntSet = Set.Make(Int)

type t = {
  generation_size : int;
  mutation_percent : float;
  gps: int;
  mutable population : (Strategy.t * float) array;
}

(** [init_strategy gps i] is [(s, f)], where [s] is a randomly
    initialized strategy and [f] is its fitness score after playing [gps]
    tetris games. *)
let init_strategy gps _ = 
  let s = Strategy.initialize () in
  s, train s gps

let initialize gen_size mut_pct gps = {
  generation_size = gen_size;
  mutation_percent = mut_pct;
  gps = gps;
  population = Array.init gen_size (init_strategy gps);
}

(** [select_index length set] is a random number between 0 and [length] - 1, 
    inclusive, such that the number is not in [set]. *)
let select_index length set =
  let i = ref (Random.int length) in
  while IntSet.mem !i set do 
    if !i = length - 1 then i := 0 else i := !i +1
  done;
  !i 

(** [one_tenth arr] is an array consisting of a random 10 percent of the 
    elements in [arr]. *)
let one_tenth arr  =
  let result = ref [] in
  let set = ref IntSet.empty in
  let length = Array.length arr in 
  let ten_percent = length |> float_of_int |> ( *. ) 0.1 |> int_of_float in
  while IntSet.cardinal !set < ten_percent do 
    let i = select_index length !set in 
    set := IntSet.add i !set;
    result := arr.(i) :: (!result)
  done;
  Array.of_list !result

(** [two_fittest arr] is a tuple of the two strategies in [arr] that have the 
    highest score, and if there are multiple strategies that rank in the top 
    two, then the first two in [arr] are chosen.
    Raises: [invalig_arg] if [arr] has less than 2 elements. *)
let two_fittest arr = 
  if Array.length arr < 2 then invalid_arg "arr";
  let cmp s1 s2 = compare (snd s2) (snd s1) in 
  Array.sort cmp arr;
  arr.(0), arr.(1)

let tournament_selection s =
  let children = ref [] in
  for i = 0 to s.generation_size / 3 - 1 do 
    let parents = s.population |> one_tenth |> two_fittest in 
    let parent_one = fst parents in 
    let parent_two = snd parents in  
    let child =  begin
      match parent_one, parent_two with
      | (s1, f1), (s2, f2) -> crossover s1 s2 f1 f2
    end in 
    children := (child, train ~debug: true child s.gps) :: !children
  done;
  s.population <- !children |> Array.of_list |> Array.append s.population;
  s

let delete_last s =
  let current_pop = s.population |> Array.length |> float_of_int in
  let original_pop = 1.3 |> ( /.) current_pop in 
  let del = original_pop |> ( *. ) 0.3 |> int_of_float in
  let cmp s1 s2 = compare (snd s1) (snd s2) in 
  Array.sort cmp s.population;
  s.population <- Array.sub s.population del (int_of_float original_pop);
  s

let mutate s = 
  let mutate_aux (strategy, fitness)= 
    if s.mutation_percent <> 0.0 && Random.float 1.0 <= s.mutation_percent 
    then (Strategy.mutate strategy, fitness)
    else (strategy, fitness) in
  let mutated = Array.map mutate_aux s.population in 
  s.population <- mutated;
  s

(** [gene_to_string (s, f)] is a string representation of [(s, f)]. *)
let gene_to_string (s, f) =
  "(" ^ pp_strategy s ^ ", " ^ string_of_float f ^ ")"

let display s =
  let disp = s.population |> pp_array gene_to_string in 
  print_endline disp

let max_strategy acc s =
  if snd acc >= snd s then acc else s

let get_best_strategy s =
  s.population |> Array.fold_left max_strategy s.population.(0)

let generation ?display:(d=false) s =
  let gen = s |> tournament_selection |> mutate |> delete_last in
  if d then display gen;
  gen

let pop = initialize 30 3.0 2 |> generation ~display: true