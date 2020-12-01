type t = {
  generation_size : int;
  mutation_percent : float;
  mutable population : Strategy.t array;
  mutable max_score : float;
}

let initialize gen_size mut_pct = {
  generation_size = gen_size;
  mutation_percent = mut_pct;
  population = Array.make gen_size (Strategy.initialize ());
  max_score = 0.0;
}

let tournament_selection s =
  failwith "Unimplemented"

let delete_last s =
  failwith "Unimplemented"

let generation s =
  failwith "Unimplemented"

let display s =
  failwith "Unimplemented"