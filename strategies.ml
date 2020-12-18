open Strategy

module IntSet = Set.Make(Int)

type t = {
  generation_size : int;
  mutation_percent : float;
  mutable population : (Strategy.t * float) array;
  mutable max_score : float;
}

let initialize gen_size mut_pct = {
  generation_size = gen_size;
  mutation_percent = mut_pct;
  population = Array.init gen_size (fun _ -> (Strategy.initialize (), 0.0));
  max_score = 0.0;
}

(** [select_index length set] is a random number between 0 and [length] - 1, 
    inclusive, such that the number is not in [set]. *)
let select_index length set =
  let i = ref (Random.int length) in
  while IntSet.mem !i set do 
    if !i = length - 1 then i := 0 else i := !i +1
  done;
  !i 

(** [one_tenth arr] is an array consisting of a random 10 percent of the elements
    in [arr]. *)
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
    highest score, and if there are multiple strategies that rank in the top two, 
    then the first two in [arr] are chosen. *)
let two_fittest arr = 
  let cmp s1 s2 = compare (snd s2) (snd s1) in 
  Array.sort cmp arr;
  (arr.(0), arr.(1))

let tournament_selection s =
  let children = ref [] in
  for i = 0 to 299 do 
    let parents = s.population |> one_tenth |> two_fittest in 
    let parent_one = fst parents in 
    let parent_two = snd parents in  
    let child =  begin
      match parent_one, parent_two with
      | (s1, f1), (s2, f2) -> crossover s1 s2 f1 f2
    end in 
    children := (child, train child 10) :: !children
  done;
  s.population <- !children |> Array.of_list |> Array.append s.population;
  s

let delete_last s =
  let currrent_pop = s.population |> Array.length |> float_of_int in
  let original_pop = 1.3 |> ( /.) currrent_pop in 
  let del = original_pop |> ( *. ) 0.3 |> int_of_float in
  let cmp s1 s2 = compare (snd s1) (snd s2) in 
  Array.sort cmp s.population;
  s.population <- Array.sub s.population del (int_of_float original_pop);
  s

let mutate s = 
  let mutate_aux (strategy, fitness)= 
    if Random.float 1. <= s.mutation_percent 
    then (Strategy.mutate strategy, fitness)
    else (strategy, fitness) in
  let mutated = Array.map mutate_aux s.population in 
  s.population <- mutated;
  s

let display s =
  failwith "Unimplemented"

let generation ?display:(d=false) s =
  let gen = s |> tournament_selection |> mutate |> delete_last in
  if d then display gen;
  gen