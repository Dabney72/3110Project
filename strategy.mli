(** The type [t] is a strategy for each block, which is a 4d vector of floats
    (a, b, c, d). The strategy uses these parameters to calculate a score for
    each possible move, then chooses the move with the highest score. *)
type t

(** [initialize ()] initializes the strategy, with weights a, c, and d 
    initialized to a random number between 0 and -1, and b to a random number
    between 0 and 1. The vector is then normalized. *)
val initialize : unit -> t

(** [play_game s] plays a tetris game with the strategy [s]. *)
val play_game : t -> State.t -> unit

(** [fitness s] is the total number of lines cleared for the strategy [s]. 
    The higher the fitness, the better the strategy. *)
val fitness : t -> float

(** [crossover s1 s2] combines strategies [s1] and [s2] by computing
    [s] = [s1 * fitness(st1) + s2 * fitness(s2)]. Thus, it is a weighted
    average, with more weight on the parent with the greater fitness score. *)
val crossover : t -> t -> t

(** [mutate s] is [s] with a random component of its vector adjusted by a
    random amount up to + or - 0.2, and then normalized. *)
val mutate : t -> t

(** [score s m] is the rating that strategy [s] gives the move [m]. A high
    score indicates that [s] thinks [m] is a good move. *)
val score : t -> Move.t -> float

