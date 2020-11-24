type t

val new_g: int -> int -> t

val get_pos: t -> int * int

val move: t -> int * int -> unit

val rand_dir: int * int