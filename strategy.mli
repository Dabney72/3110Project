(** The type [t] is a strategy for each block, which is a 4d vector of floats
    (a, b, c, d). The strategy uses these parameters to calculate a score for
    each possible move, then chooses the move with the highest score. *)
type t

(** [initialize ()] initializes the strategy, with weights a, c, and d 
    initialized to a random number between 0 and -1, and b to a random number
    between 0 and 1. The vector is then normalized. *)
val initialize : unit -> t

(** [crossover s1 s2] combines strategies [s1] and [s2] by computing
    [s] = [fitness(s1) * s1 + fitness(s2) * s2]. Thus, it is a weighted
    average of vectors , with more weight on the parent with the greater
    fitness score. *)
val crossover : t -> t -> t

(** [mutate s] is [s] with a random component of its vector adjusted by a
    random amount up to + or - 0.2, and then normalized. *)
val mutate : t -> t

(** [score s o] is the rating that strategy [s] gives the move outcome [o], 
    calculated by (a * aggregate height) + (b * complete lines)
    + (c * holes) + (d * bumpiness), where a,b,c,d are the weights of [s].
    A high score indicates that [s] thinks [o] is a good outcome. *)
val score : t -> float list -> float

(** [play_game s] plays a tetris game with the strategy [s], and returns
    the number of lines that were cleared as a float. *)
val play_game : t -> float
