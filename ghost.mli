type t

(** [new_g] is a new ghost with the given initial position. *)
val new_g: int -> int -> int * int -> t

(** [get_pos] is the position of a ghost as a tuple. *)
val get_pos: t -> int * int

(** [move] changes a ghost's position by the tuple given. For example, 
    move ghost_test (50,0) will update the position of ghost_test to be to the 
    right by 50. *)
val move: t -> int * int -> unit


val prev_move: t -> int *int 

(** [is_following] is true if the ghost is currently following the user and 
    false otherwise. *)
val is_following: t -> bool

(** [following_counter] is the number of moves that have passed since the 
    ghost started following the user. *)
val following_counter: t -> int

(** [incr_following_count] increments the [following_counter] of a ghost. *)
val incr_following_count: t -> unit

(** [reset_following] resets the following counter to 0 and marks the ghost as 
    no longer following the ghost. *)
val reset_following: t -> unit

val start_following: t -> unit

val set_prev_move: t -> int * int -> unit