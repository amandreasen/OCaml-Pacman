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

(** A [fruit] contains a fruit sprite and the number of points the fruit is 
    worth. *)
type fruit = {
  sprite: Sprite.t;
  points: int; 
}

(** [make_map point name fruit] returns a Map.t with a bottom left corner at 
    [point] and a tile layout according to the map name [name]. The map will
    generate the fruit [fruit] after a certain amount of food has been eaten.
    Fails if [name] is not a valid map name (current valid map names are 
    "standard", "OCaml", and "3110")*)
val make_map: Constants.point -> string -> fruit -> t

(**[draw_map map color] will draw the map [map] to the active GUI window
   with the wall color [color]. *) 
val draw_map: t -> Graphics.color -> unit

(**[draw_food map] will draw all of the current food in [map] to the active
   GUI window. *) 
val draw_food: t -> unit

(**[check_move pos map dir initialized] checks if the object with current
   position [pos] can move in a particular [map] in a certain direction [dir]. 
   It cannot move into the ghost tiles if it is [initialized].  *)
val check_move: Constants.point -> t -> Constants.point -> bool -> bool

(**[get_tile_type pos map] checks the type of the tile given the [pos] and 
    the [map]. The tile type is returned as a string. *)
val get_tile_type: Constants.point -> t -> string

(**[check_food point map] will check if the tile in [map] at pixel position
   [point] contains food. If it is, the function will update that tile to be 
   empty. Otherwise, the function does nothing.*)
val check_food: Constants.point -> t -> unit

(**[get_tile_value point map] will get the tile point value of the tile at 
   the pixel position [point] in the map [map]. *) 
val get_tile_value: Constants.point -> t -> int

(**[generate_fruit map] will randomly place a fruit tile on an empty tile in 
   the map [map]. If there are no empty tiles, this function does nothing.*) 
val generate_fruit: t -> unit

(**[food_count map] will return the total number of food and special tiles 
   in the map [map]. *)
val food_count: t -> int

(**[clear_fruit map] will clear any remaining fruit tiles from the map [map]. *) 
val clear_fruit: t -> unit

(**[generate_special map] will generate the special food given the [map]. *)
val generate_special: t -> unit

(**[initial_ghost_moves map] will generate the initial movement of the ghosts 
   based on the [map]. *)
val initial_ghost_moves: t -> Constants.point array array 

(**[ghost_init_positions map] will generate the initial position of each ghost
   based on the [map]. *)
val ghost_init_positions: t -> Constants.point array

val get_corner: t -> Constants.point -> Constants.point