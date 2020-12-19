(** A module for the Tetris A.I. strategy. *)

(** The type [t] is a strategy for each block, which is a 4d vector of floats
    (a, b, c, d). The strategy uses these parameters to calculate a score for
    each possible move, then chooses the move with the highest score. *)
type t

(** [initialize ()] initializes the strategy, with weights a, c, and d 
    initialized to a random number between 0 and -1, and b to a random number
    between 0 and 1. The vector is then normalized. *)
val initialize : unit -> t

(** [crossover s1 s2 f1 f2] combines strategies [s1] and [s2] by computing
    [s] = [f1 * s1 + f2 * s2], where [f1] is the total number of lines cleared
    (fitness) by [s1], and [f2] is the fitness of [s2]. *)
val crossover : t -> t -> float -> float -> t

(** [mutate s] is [s] with a random component of its vector adjusted by a
    random amount up to +- 0.2, and then normalized. *)
val mutate : t -> t

(** [score s o] is the rating that strategy [s] gives the move outcome [o], 
    calculated by [(a * aggregate height) + (b * complete lines)
    + (c * holes) + (d * bumpiness)], where a,b,c,d are the weights of [s].
    A high score indicates that [s] thinks [o] is a good outcome. *)
val score : t -> float list -> float

(** [move_next_piece s st] moves and drops the upcoming piece in [st] according
    to the strategy [s]. *)
val move_next_piece : t -> State.t -> unit

(** [play_random_game s] plays a tetris game with the strategy [s], and returns
    the number of lines that were cleared as a float. *)
val play_random_game : t -> float

(** [train s x] is the average score obtained by the strategy [s] after
    playing [x] random tetris games. *)
val train : t -> int -> float 

(**/**)

(* These helper function are for testing purposes only and therefore are 
   excluded from the documentation.*)

(** [init_with_weights a b c d] is a strategy vector with the weights [a], [b],
    [c], and [d]. The result is then normalized. *)
val init_with_weights : float -> float -> float -> float -> t

(** [to_list s] is [s] as a float list. *)
val to_list : t -> float list

(**/**)