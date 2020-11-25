open Ghost
type t 

(** [get_points] is the current player's points. *)
val points: t -> int

(** [lives] is the number of lives the currrent player has left*)
val lives: t -> int

(** [ghosts] is the array of ghosts in the current game *)
val ghosts: t -> Ghost.t array

(** [current_level] is the current level of the player *)
val current_level: t -> int

(** [initial_state] initialies a new state for a new player *)
val initial_state: Map.t -> Ghost.t array -> t

val update_state_food: t -> Map.t -> t

(** [make_ghosts] is the array of n number of ghosts where the second and third 
    inputs are the minimum x and y values, respectively. Initially the ghosts 
    are all lined up in a row horizontally starting from the minimum x and y 
    values. 
    val make_ghosts: int -> int -> int -> Ghost.t array *)