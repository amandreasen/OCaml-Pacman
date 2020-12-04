open Graphics

(* Constants *)
let tile_size = 50
let wall_width = 20
let food_radius = 3
let special_radius = 6
let food_color = rgb 255 184 245
let wall_color = Graphics.blue
let pacman_rad = 25

(** A [point] is of the form (x,y) and represents a pixel position in the GUI 
    window. In (x,y), x is the x-coordinate of the pixel position and y is the
    y-coordinate of the pixel position. *)
type point = int * int

(** A [coordinate] is of the form (x,y) and represents a coordinate in the map 
    tile array, where the origin is the bottom left corner of the map. Each tile 
    spans one vertical and horizontal unit. In (x,y), x is the x-coordinate of
    the position and y is the y-coordinate of the position.*) 
type coordinate = int * int

(** An [orientation] represents a cardinal direction. *)  
type orientation = Top | Bot | Left | Right

(** A [corner] is represented by a pair of orientations (y, x), where y is the 
    vertical orientation (one of Top or Bot) and x is the horizontal orientation
    (one of Left or Right). *)
type corner = orientation * orientation 

(** A [wall] represents a wall tile in the map. A standard wall is vertical 
    (Vert) or Horizontal (Horz). A wall may also be a corner (Corner), or it may
    be a wall end (End), meaning that it is capped at one end. *) 
type wall = Vert | Horz | Corner of corner | End of orientation

(** A [tile] represents tiles in the map. Empty tiles do not have anything. Food
    tiles have food in their centers. Special tiles have special food in their
    centers that give the player certain powerups. Ghost tiles do not have 
    anything but may only be traversed by ghosts. Wall tiles have a wall type 
    and may not be traversed by players or ghosts. Players can traverse empty, 
    food, and special tiles. Ghosts can traverse empty, food, special, and ghost
    tiles.  *)
type tile = Empty | Food | Special | Ghost | Wall of wall

(** A [map_tile] represents a tile in the map. A map_tile has a tile_type of type
    tile and a bottom_left position of type point. *) 
type map_tile = 
  {
    tile_type: tile;
    bottom_left: point;
  }

(** A Map.t type represents a game map. It has a tiles field that is a 2D array
    with elements of type map_tile, representing all tiles in the map. The map
    additionally has an integer width and integer height, as well as a 
    bottom_left field of type point so that the map can be appropriately drawn 
     in the GUI. *)
type t =
  {
    tiles: map_tile array array;
    width: int;
    height: int;
    bottom_left: point;
  }

(** The standard_map is a Map.t type and represents a standard game map with
    standard dimensions. *)  
let standard_map = 
  {
    tiles = [||];
    width = 1300;
    height = 550;
    bottom_left = (0,0)
  } 

(** The standard_maze is a 2D array of tile_types that represents the layout
    of the default game map. *) 
let standard_maze = 
  [|
    [|Wall (Corner (Bot, Left)); Wall Vert; Wall Vert; Wall Vert; Wall Vert; 
      Wall Vert; Wall Vert; Wall Vert; Wall Vert; Wall Vert; 
      Wall(Corner (Top, Left))|];
    [|Wall Horz; Food; Food; Food; Food; Food; Food; Food; Food; Food; 
      Wall Horz|];
    [|Wall Horz; Food; Wall (Corner (Bot, Left)); Wall Vert; Wall (End Top); 
      Food; Wall (End Bot); Wall Vert; Wall (Corner (Top, Left)); Food; 
      Wall Horz|];
    [|Wall Horz; Food; Wall (End Right); Food; Food; Food; Food; Food; 
      Wall (End Right); Food; Wall Horz|];
    [|Wall Horz; Food; Food; Food; Wall (Corner (Bot, Left)); Wall Vert; 
      Wall (Corner (Top, Left)); Food; Food; Food; Wall Horz|];
    [|Wall Horz; Food; Wall (End Left); Food; Wall Horz; Food;
      Wall Horz; Food; Wall (End Left); Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall (End Right); Food; 
      Wall (End Right); Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall (End Right); Food; Food; Food; Food; Food;
      Wall (End Right); Food; Wall Horz|];
    [|Wall Horz; Food; Food; Food; Wall (End Bot); Wall Vert; Wall
        (End Top); Food; Food; Food; Wall Horz|];
    [|Wall Horz; Food; Wall (End Left); Food; Food; Food; Food; Food; Wall 
        (End Left); Food; Wall Horz|];    
    [|Wall Horz; Food; Wall Horz; Food; Wall (Corner (Bot, Left)); Wall Vert; 
      Wall (Corner (Top, Left)); Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall Horz; Ghost; Wall (End Right); 
      Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall Horz; Ghost;
      Ghost; Food; Wall Horz; Food; Wall Horz;|];
    [|Wall Horz; Food; Wall Horz; Food; Wall Horz; Ghost;
      Ghost; Food; Wall Horz; Food; Wall Horz;|];
    [|Wall Horz; Food; Wall Horz; Food; Wall Horz; Ghost; 
      Wall (End Left); Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; 
      Wall (Corner (Bot, Right)); Wall Vert; Wall (Corner (Top, Right)); 
      Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall (End Right); Food; Food; Food; Food; Food; 
      Wall (End Right); Food; Wall Horz|];
    [|Wall Horz; Food; Food; Food; Wall (End Bot); Wall Vert; Wall
        (End Top); Food; Food; Food; Wall Horz|];
    [|Wall Horz; Food; Wall (End Left); Food; Food; Food; Food; Food;
      Wall (End Left); Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall (End Left); Food; 
      Wall (End Left); Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall (End Right); Food; Wall Horz; Food;
      Wall Horz; Food; Wall (End Right); Food; Wall Horz|];
    [|Wall Horz; Food; Food; Food; Wall (Corner (Bot, Right)); Wall Vert; 
      Wall (Corner (Top, Right)); Food; Food; Food; Wall Horz|];
    [|Wall Horz; Food; Wall (End Left); Food; Food; Food; Food; Food; 
      Wall (End Left); Food; Wall Horz|];
    [|Wall Horz; Food; Wall (Corner (Bot, Right)); Wall Vert; Wall (End Top); 
      Food; Wall (End Bot); Wall Vert; Wall (Corner (Top, Right)); Food; 
      Wall Horz|];  
    [|Wall Horz; Food; Food; Food; Food; Food; Food; Food; Food; Food; 
      Wall Horz|];
    [|Wall (Corner (Bot, Right)); Wall Vert; Wall Vert; 
      Wall Vert; Wall Vert; Wall Vert; Wall Vert; Wall Vert;
      Wall Vert;Wall Vert; Wall(Corner (Top, Right))|];
  |]

(** The ocaml_maze is a 2D array of tile types that represents the layout of
    an OCaml game map.*) 
let ocaml_maze = 
  [| 
    [|Wall (Corner (Bot, Left)); Wall Vert; Wall Vert; Wall Vert;
      Wall Vert; Wall Vert; Wall Vert; Wall Vert; Wall Vert;
      Wall Vert; Wall(Corner (Top, Left))|];
    [|Wall Horz; Food; Food; Food; Food; Food; Food; Food; Food; Food; 
      Wall Horz|];
    [|Wall Horz; Food; Wall (End Left); Food; Wall (Corner (Bot, Left)); 
      Wall Vert; Wall (Corner (Top, Left)); Food; Wall (End Left); Food;
      Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall Horz; Empty; Wall Horz; Food; 
      Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall (Corner (Bot, Right)); Wall Vert; 
      Wall (Corner (Top, Right)); Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Food; Food; Food; Food; Wall Horz; 
      Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall (Corner (Bot, Left)); Wall Vert; 
      Wall (Corner (Top, Left)); Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall (End Right); Food; Wall Horz; Food; Wall Horz;
      Food; Wall (End Right); Food; Wall Horz|];
    [|Wall Horz; Food; Food; Food; Wall (End Right); Food; Wall (End Right);
      Food; Food; Food; Wall Horz|];
    [|Wall Horz; Food; Wall (End Left); Food; Food; Food; Food; Food; Wall (
          End Left); Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall (End Bot); Wall Vert; 
      Wall (Corner (Top, Left)); Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Ghost; Ghost; Wall Horz; Food; Wall 
        Horz; Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Ghost; Ghost; Wall Horz; Food; 
      Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall (End Bot); Wall Vert; 
      Wall (Corner (Top, Right)); Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Food; Food; Food; Food; Wall Horz; Food; 
      Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall (End Bot); Wall Vert; 
      Wall (Corner (Top, Left)); Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall (End Right); Food; Food; Food; Wall Horz; Food; 
      Wall (End Right); Food; Wall Horz|];
    [|Wall Horz; Food; Food; Food; Wall (End Bot); Wall Vert; Wall Horz; Food; 
      Food; Food; Wall Horz|];
    [|Wall Horz; Food; Wall (End Left); Food; Food; Food; Wall Horz; Food; 
      Wall (End Left); Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall (End Bot); Wall Vert; 
      Wall (Corner (Top, Right)); Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Food; Food; Food; Food; Wall Horz; Food;
      Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall (Corner (Bot, Left)); Wall Vert; 
      Wall (End Top); Food; Wall Horz; Food; Wall Horz|];
    [|Wall Horz; Food; Wall Horz; Food; Wall Horz; Food; Food; Food; Wall Horz;
      Food; Wall Horz|];
    [|Wall Horz; Food; Wall (End Right); Food; Wall (End Right); Food; Food; Food; 
      Wall (End Right); Food; Wall Horz|];
    [|Wall Horz; Food; Food; Food; Food; Food; Food; Food; Food; Food;
      Wall Horz|];
    [|Wall (Corner (Bot, Right)); Wall Vert; Wall Vert; Wall Vert; Wall Vert;
      Wall Vert; Wall Vert; Wall Vert; Wall Vert;Wall Vert; 
      Wall(Corner (Top, Right))|];
  |]

let check_contains pos bottom_left = 
  ((fst) pos + pacman_rad <= (fst) bottom_left + tile_size) &&
  ((fst) pos - pacman_rad <= (fst) bottom_left) &&
  ((snd) pos + pacman_rad <= (snd) bottom_left + tile_size) &&
  ((snd) pos - pacman_rad <= (snd) bottom_left) 


let get_tile_type2 pos (tile_array:map_tile array) = 
  let h_list = Array.to_list tile_array in
  let rec check_tile (list:map_tile list) =
    match list with
    | []-> ""
    | h::t ->
      if check_contains pos ((*position_to_coordinate*) h.bottom_left) then
        match h.tile_type with
        | Wall _ -> "Wall"
        | Empty -> "Empty"
        | Food -> "Food"
        | Special -> "Special"
        | Ghost -> "Ghost"

      else check_tile t in
  check_tile h_list

let get_tile_type pos map=   
  (* The position of the pacman is the center of the circle, each time it moves 
     1/5 of a tile*)
  let map_list = Array.to_list map.tiles in
  let rec check_main map_l=
    match map_l with
    | []-> ""
    | h::t -> 
      match (get_tile_type2 pos h) with
      | "" -> check_main t
      | str-> str
  in
  check_main map_list

let check_move2 pos (tile_array:map_tile array) = 
  let h_list = Array.to_list tile_array in
  let rec check_tile (list:map_tile list) =
    match list with
    | []-> ""
    | h::t ->
      if check_contains pos ((*position_to_coordinate*) h.bottom_left) then
        match h.tile_type with
        | Wall _ -> "Wall"
        | Empty -> "Empty"
        | Food -> "Food"
        | Special ->"Special"
        | Ghost -> "Ghost"

      else check_tile t in
  check_tile h_list

let check_move pos map dir=   
  (* The position of the pacman is the center of the circle, each time it moves 
     1/5 of a tile*)
  let new_pos = (fst pos + fst dir, snd pos + snd dir) in
  let map_list = Array.to_list map.tiles in
  let rec check_main map_l=
    match map_l with
    | []-> false
    | h::t -> 
      match (check_move2 new_pos h) with
      | "Wall"-> false
      | ("Empty" | "Food" | "Special"| "Ghost") -> true
      | _ -> check_main t
  in
  check_main map_list

let check_food_tile pos map=   
  (* The position of the pacman is the center of the circle, each time it moves 
     1/5 of a tile*)
  let map_list = Array.to_list map.tiles in
  let acc=0 in
  let rec check_main map_l=
    match map_l with
    | []-> acc
    | h::t ->
      match (check_move2 pos h) with
      | "Food"-> acc+1
      | ("Empty" | "Wall" | "Special"| "Ghost") -> acc
      | _ -> check_main t
  in
  check_main map_list

let check_contains2 pos bottom_left = 
  ((fst) pos + pacman_rad <= (fst) bottom_left + tile_size) &&
  ((fst) pos - pacman_rad <= (fst) bottom_left) &&
  ((snd) pos + pacman_rad <= (snd) bottom_left + tile_size) &&
  ((snd) pos - pacman_rad <= (snd) bottom_left) 

(** [position_to_coordinate p] will convert a pixel position [p] in the GUI to 
    a coordinate in the map array.
    Requires: [p] is a valid pixel position in the map.*) 
let position_to_coordinate (position: point) : coordinate = 
  let map_shift = 100 in
  let x_position = fst position - map_shift in 
  let y_position = snd position - map_shift in 
  let x_coordinate = x_position / tile_size in 
  let y_coordinate = y_position / tile_size in 
  (x_coordinate, y_coordinate)

(** [coordinate_to_position c] will convert a map coordinate [c] to a pixel
    position in the GUI window. 
    Requires: [c] is a valid coordinate in the map array.*) 
let coordinate_to_position (coordinate: coordinate) (map_corner: point) : 
  point = 
  let x_coordinate = fst coordinate in 
  let y_coordinate = snd coordinate in 
  let map_x = fst map_corner in 
  let map_y = snd map_corner in 
  let x_position = x_coordinate * tile_size + map_x in 
  let y_position = y_coordinate * tile_size + map_y in 
  (x_position, y_position)

(** [check_food p m] will check if the tile at pixel position [p] in map [m] 
    is a Food tile. If it is, it will replace the Food tile with an Empty tile.
    If not, the functions does nothing.*) 
let check_food (pos: point) (map: t) =
  let coordinate = position_to_coordinate pos in 
  let x = fst coordinate in 
  let y = snd coordinate in 
  let tile = map.tiles.(x).(y) in 
  match tile.tile_type with 
  | Food -> 
    map.tiles.(x).(y) <- {tile with tile_type = Empty} 
  | _ -> ()

(** [make_tile x y map_corner tile_type] will make a map_tile with a bottom
    left corner at map coordinate [(x, y)] and a tile type of [tile_type].*)
let make_tile (x: int) (y: int) (map_corner: point) (tile_type: tile) :
  map_tile = 
  let position = coordinate_to_position (x, y) map_corner in 
  {
    tile_type = tile_type;
    bottom_left = position;
  }

(** [make_tiles x_dim y_dim map_corner tile_types] will create a 2D array
    of map_tiles representing a map of width [x_dim] and height [y_dim] with a
    bottom left corner at [map_corner] and a map layout given by [tile_types].*) 
let make_tiles (x_dim: int) (y_dim: int) (map_corner: point) 
    (tile_types : tile array array): map_tile array array = 
  let default_tile = 
    {
      tile_type = Empty;
      bottom_left = map_corner;
    }
  in
  let tile_array = Array.make_matrix y_dim x_dim default_tile in
  for i = 0 to y_dim - 1 do 
    for j = 0 to x_dim - 1 do 
      tile_array.(i).(j) <- make_tile i j map_corner tile_types.(i).(j)
    done;
  done;
  tile_array

(** [make_map corner maze_name] will return a map with a bottom left corner at 
    [corner] and a tile layout represented by the maze label [maze_name]. Fails
    if [maze_name] is not a valid maze name.*)
let make_map (corner: point) (maze_name: string) : t =   
  let tile_list = 
    match maze_name with 
    | "standard" -> make_tiles 11 26 corner standard_maze
    | "OCaml" -> make_tiles 11 26 corner ocaml_maze
    | _ -> failwith "map not found"
  in
  match maze_name with
  | "standard" -> {standard_map with bottom_left = corner; tiles = tile_list}
  | "OCaml" -> {standard_map with bottom_left = corner; tiles = tile_list}
  | _ -> failwith "map not found"
(** [draw_corner_single first second] will draw a corner to the GUI window 
    with one endpoint of point [first] and the other endpoint at point [second]. 
*) 
let draw_corner_single (first: point) (second: point) : unit = 
  let fst_x = fst first in 
  let fst_y = snd first in 
  let snd_x = fst second in 
  let snd_y = snd second in 
  moveto fst_x fst_y;
  lineto fst_x snd_y;
  lineto snd_x snd_y

(* [draw_corner_double] will draw a double corner to the GUI window with an 
   inner corner with endpoints [first] and [second] and a corner of orientation 
   [corner_type]. *)
let draw_corner_double (first: point) (second: point) (corner_type: corner) :
  unit = 
  draw_corner_single first second;
  let fst_x = fst first in 
  let fst_y = snd first in 
  let snd_x = fst second in 
  let snd_y = snd second in 
  let first_x = 
    if snd corner_type = Left 
    then fst_x + wall_width 
    else fst_x - wall_width 
  in 
  let second_y = 
    if fst corner_type = Top 
    then snd_y - wall_width 
    else snd_y + wall_width 
  in 
  draw_corner_single (first_x, fst_y) (snd_x, second_y)

(**[draw_corner_full] will draw a double_corner to the GUI window in the tile
   [tile] with a corner orientation of [corner_type]. *)
let draw_corner_full (tile: map_tile) (corner_type: corner) : unit = 
  let tile_x = fst tile.bottom_left in 
  let tile_y = snd tile.bottom_left in 
  let corner_horz = snd corner_type in 
  let corner_vert = fst corner_type in 
  let margin = (tile_size - wall_width) / 2 in 
  let fst_x = if corner_horz = Left then margin else margin + wall_width in 
  let fst_y = if corner_vert = Top then 0 else tile_size in 
  let snd_x = if corner_horz = Left then tile_size else 0 in 
  let snd_y = if corner_vert = Top then tile_size - margin else margin in 
  let fst = (fst_x + tile_x, fst_y + tile_y) in 
  let snd = (snd_x + tile_x, snd_y + tile_y) in 
  draw_corner_double fst snd corner_type

(**[calculate_coordinates dir margin tile_x tile_y shift] will calculate the
   endpoints for a wall_end tile of orientation [dir] with a tile margin of 
   [margin] and bottom_left corner of [(tile_x, tile_y)]. The wall end will be
   a pixel distance of [shift] from the tile side matching its orientation.*)
let calculate_coordinates (dir: orientation) (margin: int) (tile_x: int)
    (tile_y: int) (shift: int) : point * point = 
  let margin_fst = margin in 
  let margin_snd = tile_size - margin in 
  let shift_comp = tile_size - shift in
  match dir with 
  | Top -> 
    let fst = (tile_x + margin_fst, tile_y) in 
    let snd = (tile_x + margin_snd, tile_y) in 
    (fst, snd)
  | Bot -> 
    let fst = (tile_x + margin_fst, tile_y + shift_comp) in 
    let snd = (tile_x + margin_snd, tile_y + shift_comp) in
    (fst, snd)
  | Left -> 
    let fst = (tile_x + shift_comp, tile_y + margin_fst) in
    let snd = (tile_x + shift_comp, tile_y + margin_snd) in 
    (fst, snd)
  | Right -> 
    let fst = (tile_x, tile_y + margin_fst) in 
    let snd = (tile_x, tile_y + margin_snd) in 
    (fst, snd)

(**[draw_wall_lines first second shift] will draw a double wall with the
   endpoint of line at [first] and the parallel endpoint of the other line at 
   [second]. The length of the wall is determined by [shift]. *)
let draw_wall_lines (first: point) (second: point) (shift: int) : unit = 
  let fst_x = fst first in 
  let fst_y = snd first in 
  let snd_x = fst second in 
  let snd_y = snd second in 

  moveto fst_x fst_y;
  let endpoint_x = if fst_x = snd_x then fst_x + shift else fst_x in 
  let endpoint_y = if fst_y = snd_y then fst_y + shift else fst_y in 
  lineto endpoint_x endpoint_y;

  moveto snd_x snd_y;
  let endpoint_x = if fst_x = snd_x then snd_x + shift else snd_x  in 
  let endpoint_y = if fst_y = snd_y then snd_y + shift else snd_y in 
  lineto endpoint_x endpoint_y

(**[draw_wall_cap tile_x tile_y dir shift] will draw a wall cap for a wall 
   end tile for the tile with bottom left corner [(tile_x, tile_y)] and 
   an orientation of [dir]. The location of the wall cap within the tile is
   determined by [shift].*)
let draw_wall_cap (tile_x: int) (tile_y: int) (dir: orientation) 
    (shift: int) : unit = 
  let margin = (tile_size - wall_width) / 2 in 
  let fst = margin in 
  let snd = tile_size - margin in 
  let shift_comp = tile_size - shift in
  match dir with 
  | Top -> 
    let y_pos = tile_y + shift in
    moveto (tile_x + fst) y_pos;
    lineto (tile_x + snd) y_pos
  | Bot -> 
    moveto (tile_x + fst) (tile_y + shift_comp);
    lineto (tile_x + snd) (tile_y + shift_comp);
  | Left -> 
    moveto (tile_x+ shift_comp) (tile_y + fst);
    lineto (tile_x+ shift_comp) (tile_y + snd);
  | Right -> 
    let x_pos = tile_x + shift in 
    moveto x_pos (tile_y + fst);
    lineto x_pos (tile_y + snd)

(**[draw_wall_end tile end_type] will draw a wall end tile at the tile [tile]
   with orientation [end_type]. *) 
let draw_wall_end (tile: map_tile) (end_type: orientation) : unit = 
  let margin = (tile_size - wall_width) / 2 in 
  let tile_corner = tile.bottom_left in 
  let tile_x = fst tile_corner in 
  let tile_y = snd tile_corner in 
  let shift = 3 * tile_size / 4 in
  let coordinates = calculate_coordinates end_type margin tile_x tile_y shift in 
  let pos_one = fst coordinates in 
  let pos_two = snd coordinates in 
  draw_wall_lines pos_one pos_two shift;
  draw_wall_cap tile_x tile_y end_type shift

(**[draw_wall_normal tile orientation] will draw a standard horizontal or 
   vertical double-layered wall at the map tile [tile]. The direction is given 
   by [orientation] (if not Vert or Horz, the function draws nothing).
*) 
let draw_wall_normal (tile: map_tile) (orientation: wall) : unit = 
  let margin = (tile_size - wall_width) / 2 in 
  let tile_corner = tile.bottom_left in 
  let tile_x = fst tile_corner in 
  let tile_y = snd tile_corner in 
  let first = margin in 
  let second = tile_size - margin in 
  match orientation with 
  | Vert -> 
    let fst = (tile_x + first, tile_y) in 
    let snd = (tile_x + second, tile_y) in
    draw_wall_lines fst snd tile_size 

  | Horz -> 
    let fst = (tile_x, tile_y + first) in 
    let snd = (tile_x, tile_y + second) in
    draw_wall_lines fst snd tile_size 

  | _ -> ()

(**[draw_wall tile wall_type] draws a wall of type [wall_type] in the map 
   tile [tile] in the GUI window. *) 
let draw_wall (tile: map_tile) (wall_type: wall) : unit = 
  set_color wall_color;
  match wall_type with 
  | Vert -> draw_wall_normal tile wall_type
  | Horz -> draw_wall_normal tile wall_type
  | Corner corner -> draw_corner_full tile corner
  | End endwall -> draw_wall_end tile endwall

(**[draw_food tile radius] will draw food in the map tile [tile] with a 
   radius [radius]. *) 
let draw_food_tile (tile: map_tile) (radius: int) : unit = 
  set_color food_color;
  let tile_corner = tile.bottom_left in 
  let x_center = fst tile_corner + tile_size / 2 in 
  let y_center = snd tile_corner + tile_size / 2 in 
  fill_circle x_center y_center radius

(**[draw_food_helper tile] will draw food to the tile [tile] if [tile] is a Food
   tile. Otherwise, the function does nothing. *)
let draw_food_helper (tile: map_tile) : unit = 
  match tile.tile_type with 
  | Food -> draw_food_tile tile food_radius
  | _ -> ()

(** [draw_food_row] will draw all Food tiles in the tile array [food_row] to the
    GUI window. *)
let draw_food_row (food_row: map_tile array) : unit = 
  ignore (Array.map draw_food_helper food_row);
  ()

(**[draw_food map] will draw food in all Food tiles in the map [map]. *) 
let draw_food (map: t) : unit =
  ignore (Array.map draw_food_row map.tiles);
  ()
(* temporary helper function to make map tile outlines visible *)
(* let draw_outline (tile: map_tile) : unit = 
   set_color wall_color;
   let bottom_corner = tile.bottom_left in 
   let x_position = fst bottom_corner in 
   let y_position = snd bottom_corner in 
   draw_rect x_position y_position tile_size tile_size *)

(**[draw_tile tile] will draw the correct display of the tile [tile] according
   to its tile type. Food tiles will be drawn the same as Empty tiles (the logic
   to draw food is in a separate function).  *) 
let draw_tile (tile: map_tile) : unit =
  match tile.tile_type with
  | Empty | Food | Ghost -> ()
  | Special -> draw_food_tile tile special_radius
  | Wall wall -> draw_wall tile wall

(**[draw_map_row] will draw the correct display of all tiles in the tile 
   row [map_row] according to the function [draw_tile]. *) 
let draw_map_row (map_row : map_tile array) : unit = 
  ignore (Array.map draw_tile map_row);
  ()

(**[draw_map map] will draw the correct display of all tiles in the map [map]
   according to the function [draw_tile].*) 
let draw_map (map: t) : unit = 
  ignore (Array.map draw_map_row map.tiles);
  ()