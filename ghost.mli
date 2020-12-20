open Sprite

type t

(** [new_ghost x y dir color] returns a new ghost with the initial pixel 
    x-coordinate [x], the initial pixel y-coordinate [y], the initial direction 
    [dir], and the color [color]. Other values for the ghost fields will be 
    initialized appropriately.*)
val new_ghost: int -> int -> int * int -> string -> t

(** [get_position ghost] is the position of the [ghost] as a tuple (x,y). *)
val get_position: t -> int * int

(** [move ghost dir] changes the position of ghost [ghost] by the tuple [dir]
    given. For example, move ghost_test (50,0) will update the position of
    ghost_test to be to the right by 50. *)
val move: t -> int * int -> unit

(** [prev_move ghost] is the previous move that the ghost [ghost] made as a 
    tuple (x, y), where [x] is the number of pixels moved in the x-direction and 
    [y] is the number of pixels moved in the y-direction. *)
val prev_move: t -> int * int 

(** [is_following ghost] returns true if the ghost [ghost] is currently 
    following the user and false otherwise. *)
val is_following: t -> bool

(** [following_counter ghost] is the number of moves that have passed since the 
    ghost [ghost] started following the user. *)
val following_counter: t -> int

(** [incr_following_count ghost] increments the amount of time [ghost] has been
    following the player. *)
val incr_following_count: t -> unit

(** [reset_following ghost] resets the amount of time [ghost] has been following 
    the user and marks the ghost as no longer following the player. *)
val reset_following: t -> unit

(** [start_following ghost] updates the time [ghost] has been following the user 
    to be 1 time unit and marks the ghost as following the player. *)
val start_following: t -> unit

(** [get_sprite ghost] returns the sprite representing the ghost [ghost] in the 
    direction specified by the input. *)
val get_sprite: t -> Sprite.t

(** [made_move ghost] returns true if [ghost] has made a successful 
    move this turn and false otherwise. *)
val made_move: t -> bool

(** [reset_move ghost] marks [ghost] as not having made a move this turn. *)
val reset_move: t -> unit

(** [is_done_initializing ghost] is true if [ghost] has left the ghost box or 
    starting location and false otherwise. *)
val is_done_initializing: t -> bool

(** [finish_initializing ghost] sets [ghost] to be in a normal play state so 
    that it is no longer in its initialization stage where it is leaving the 
    ghost box. *)
val finish_initializing: t -> unit 

(** [move_init ghost dir] moves the ghost by [dir] when it is in its 
    initialization stage. *)
val move_init: t -> int * int -> unit 

(** [init_counter ghost] is the number of initial moves the ghost has made. *)
val init_counter: t -> int 

(** [get_state ghost] returns the state of the Ghost.t [ghost]. The state 
    may be one of "active", "scared1", "scared2", or "eaten". *) 
val get_state: t -> string 

(** [get_state ghost state] sets the state of the Ghost.t [ghost] to the state 
    [state]. [state] may be one of "active", "scared1", "scared2", or "eaten".
    Fails if [state] is not a valid state option. *) 
val set_state: t -> string -> unit