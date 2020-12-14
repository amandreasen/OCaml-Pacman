(** A [Map.t] represents a game map. It contains information about the map's
    pixel location in the GUI, its height and width dimensions, and the tiles in 
    the map.*)  
type t 

(** A [tile] represents a tile type in the map. Tiles may be empty, contain 
    food, contain special powerup food, or contain a wall. There may also be
    tiles that players cannot traverse that ghosts can.*) 
type tile

(** A [map_tile] represents a tile in the map. It contains information about
    the tile's tile type and its pixel location in the GUI window.*) 
type map_tile

(** A [point] is of the form (x,y) and represents a pixel position in the GUI 
    window. In (x,y), x is the x-coordinate of the pixel position and y is the
    y-coordinate of the pixel position. *)
type point = int * int

type fruit = {
  sprite: Sprite.t;
  points: int; 
}

(** [make_map point name] returns a Map.t with a bottom left corner at 
    [point] and a tile layout according to the map name [name]. Fails if 
    [name] is not a valid map name (current valid map names are "standard"
    and "OCaml")*)
val make_map: point -> string -> fruit -> t

(**[draw_map map] will draw the map [map] to the active GUI window. *) 
val draw_map: t -> unit

(**[draw_food map] will draw all of the current food in [map] to the active
   GUI window. *) 
val draw_food: t -> unit

val check_move: point -> t -> point -> bool

val check_move_new: point -> t -> point -> bool

val get_tile_type: point -> t -> string

(**[check_food point map] will check if the tile in [map] at pixel position
   [point] contains food. If it is, the function will update that tile to be 
   empty. Otherwise, the function does nothing.*)
val check_food: point -> t -> unit

(**[get_tile_value point map] will get the tile point value of the tile at 
   the pixel position [point] in the map [map]. *) 
val get_tile_value: point -> t -> int

(**[generate_fruit map] will randomly place a fruit tile on an empty tile in 
   the map [map]. If there are no empty tiles, this function does nothing.*) 
val generate_fruit: t -> unit

(**[food_count map] will return the total number of food and special tiles 
   in the map [map]. *)
val food_count: t -> int

(**[clear_fruit map] will clear any remaining fruit tiles from the map [map]. *) 
val clear_fruit: t -> unit