open Sprite

type t

type direction

(** [new_ghost] is a new ghost with the given initial position and initial
    move. *)
val new_ghost: int -> int -> int * int -> string -> t

(** [get_position] is the position of a ghost as a tuple. *)
val get_position: t -> int * int

(** [move] changes a ghost's position by the tuple given. For example, 
    move ghost_test (50,0) will update the position of ghost_test to be to the 
    right by 50. *)
val move: t -> int * int -> unit

(** [prev_move] is the previous move that the ghost made. *)
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
    no longer following the player. *)
val reset_following: t -> unit

(** [start_following] sets the following counter to 1 and marks the ghost as 
    following the player. *)
val start_following: t -> unit

(** [get_sprite] is the sprite representing the ghost in the direction specified
    by the input. *)
val get_sprite: t -> Sprite.t

(** [made_move] is true if the ghost has made a successful move this turn and 
    false otherwise. *)
val made_move: t -> bool

(** [reset_move] resets the ghost's [made_move] field to be false at the 
    beginning of the turn. *)
val reset_move: t -> unit

(** [is_done_initializing] is true if the ghost has left the ghost box or 
    starting location and false otherwise.  *)
val is_done_initializing: t -> bool

(** [finish_initializing] updates the ghost's fields so that it is no longer in 
    its initialization stage where it is leaving the ghost box. *)
val finish_initializing: t -> unit 

(** [move_init] moves the ghost by the specified tuple when it is in its 
    initialization stage. *)
val move_init: t -> int * int -> unit 

(** [init_counter] is the number of initial moves the ghost has made. *)
val init_counter: t -> int 

val get_state: t -> string 

val set_state: t -> string -> unit