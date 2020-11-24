type t

(** [new_g] is a new ghost with the given initial position. *)
val new_g: int -> int -> t

(** [get_pos] is the position of a ghost as a tuple. *)
val get_pos: t -> int * int

(** [move] changes a ghost's position by the tuple given. For example, 
    move ghost_test (50,0) will update the position of ghost_test to be to the 
    right by 50. *)
val move: t -> int * int -> unit

(** val rand_dir: int -> int * int *)