(** A population of different Tetris strategies. *)

(** The type t maintains a list of strategies and their fitness scores,
    a generation size, a mutation percentage, and a max fitness score. *)
type t

(** [initialize gen_size mut_pct] initializes a population of strategies
    with generation size [gen_size] and mutation percent [mut_pct]. *)
val initialize : int -> float -> t

(** [tournament_selection s] selects 10% of the population at random, and the
    two fittest individuals in this subpool proceed on for crossover to produce
    a new offspring. This process is repeated until the number of new offsprings
    produced reaches 30% of the generation size. *)
val tournament_selection : t -> t

(** [delete_last s] deletes the worst 30% of the population, to be done after
    crossovers are done. This is to be done after [tournament_selection],
    so if a generation size is 1000, [tournament_selection] will produce
    1300 strategies, and [delete_last] will reduce this back down to 1300 - 
    .3*1000 = 1000. *)
val delete_last : t -> t

(** [mutate s] mutates each strategy in [s] with the probability specified by
    [s]. *)
val mutate: t -> t

(** [display s] prints the list of strategies. *)
val display : t -> unit

(** [get_best_strategy s] is [(strat, fit)], where [strat] is the strategy in
    [s] with the highest fitness score, [fit]. *)
val get_best_strategy : t -> Strategy.t * float

(** [generation ?display s] advances [s] to the next generation,
    performing crossovers, mutations, and deletions. 
    [?display] is an optional argument for printing the output of each
    generation. It is defaulted to false. *)
val generation : ?display: bool -> t -> t