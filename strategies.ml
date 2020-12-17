open Strategy

type t = {
  generation_size : int;
  mutation_percent : float;
  mutable population : (Strategy.t * float) list;
  mutable max_score : float;
}

let initialize gen_size mut_pct = {
  generation_size = gen_size;
  mutation_percent = mut_pct;
  population = List.init gen_size (fun _ -> (Strategy.initialize (), 0.0));
  max_score = 0.0;
}

let tournament_selection s =
  failwith "Unimplemented"

let delete_last s =
  failwith "Unimplemented"

let mutate s =
  failwith "Unimplemented"

let display s =
  failwith "Unimplemented"

let generation ?display:(d=true) s =
  let gen = s |> tournament_selection |> mutate |> delete_last in
  if d then display gen;
  gen
