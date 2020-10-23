(** The abstract type is a set of commands that the player can execute
    during the game. *)
type t = 
  | Rotate
  | Move_left
  | Move_right
  | Drop
  | Hold

(** [update cmd state] mutates [state] by executing the command [cmd]. *)
val update : t -> State.t -> unit

(** [parse c] is the command given by the keyboard input [c]. *)
val parse : char -> t

