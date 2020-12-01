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

let play_game s =
  failwith "Unimplemented"

let fitness s =
  failwith "Unimplemented"

let crossover s1 s2 =
  failwith "Unimplemented"

let mutate s =
  failwith "Unimplemented"

let score s m =
  failwith "Unimplemented"