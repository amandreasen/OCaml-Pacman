type t 

type map_tile

type tile

type point = int * int

(** [map] is a list of the tiles in the current map. *)
val map: t -> map_tile array array 

(** [update_map map (x,y)] returns an updated map after a player has moved 
    to coordinates [(x,y)] in the original map [map] *)
val update_map: t -> point -> t

(** [make_map x y] is a hard-coded map with width x and height y and bottom left 
    corner at [point] *)
val make_map: point -> string -> t

val draw_map: t -> unit

val check_move: point -> t -> point -> bool

val get_tile_type: point -> t -> string

val check_food: point -> t -> unit

(* (** [clear_food] is the map after the player takes food from a tile at some 
    position *)
   val clear_food: t -> 'a * 'a -> t *)

(* temporary helper functions *)
(* val draw_tile_helper: t -> unit

   val make_map_single: int -> int -> point -> t *)