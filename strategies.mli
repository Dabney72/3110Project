(** A population of different tetris strategies. *)

(** The type t maintains a list of strategies, a generation size, a mutation
    percentage, and a max score. *)
type t

(** [initialize gen_size mut_pct] initializes a population of strategies
    with generation size [gen_size] and mutation percent [mut_pct]. *)
val initialize : int -> float -> t

(** [tournament_selection s] selects 10% of the population at random, and the
    two fittest individuals in this subpool proceed on for crossover to produce
    a new offspring. This process is repeated until the number of new offsprings
    produced reaches 30% of the population size. *)
val tournament_selection : t -> t

(** [delete_last s] deletes the worst 30% of the population, to be done after
    crossovers are done. *)
val delete_last : t -> t

(** [generation s] advances [s] to the next generation, performing crossovers,
    mutations, and deletions. *)
val generation : t -> t

(** [display s] prints the list of strategies. *)
val display : t -> unit