open Graphics

(* Idea: first run random map generation algorithm that will generate which 
   kind of tile should be at each coordinate, then write function that will map 
   coordinate to get this tile type and return the map_tile with the tile type 
   and bottom left pixel coordinate, this will be function passed to init *)

(* Constants *)
let tile_size = 50
let wall_width = 20
let food_radius = 3
let special_radius = 6
let food_color = rgb 255 184 245
let wall_color = Graphics.blue

type point = int * int

type coordinate = int * int

type orientation = Top | Bottom | Left | Right

(* (y,x) where x is horizontal orientation and y is vertical orientation *)
type corner = orientation * orientation 

type wall = Vertical | Horizontal | Corner of corner | End of orientation

type tile = Empty | Food | Special | Wall of wall

type map_tile = 
  {
    tile_type: tile;
    bottom_left: point;
  }

type t =
  {
    tiles: map_tile array array;
    width: int;
    height: int;
    bottom_left: point;
  }

let position_to_coordinate (position: point) : coordinate = 
  let x_position = fst position in 
  let y_position = snd position in 
  let x_coordinate = x_position / tile_size in 
  let y_coordinate = y_position / tile_size in 
  (x_coordinate, y_coordinate)

let coordinate_to_position (coordinate: coordinate) (map_corner: point) : 
  point = 
  let x_coordinate = fst coordinate in 
  let y_coordinate = snd coordinate in 
  let x_position = x_coordinate * tile_size in 
  let y_position = y_coordinate * tile_size in 
  (x_position, y_position) 

(* let make_corners (x_dim: int) (y_dim: int) (map_corner: point)
    (tile_array: map_tile array array) : unit = 
   let corner_x = fst map_corner in 
   let corner_y = snd map_corner in 
   let right_x = corner_x + (x_dim - 1) * tile_size in 
   let top_y = corner_y + (y_dim - 1) * tile_size in 
   tile_array.(0).(0) <- 
    {tile_type = Wall (Corner (Left, Bottom)); bottom_left = map_corner};
   tile_array.(y_dim - 1).(0) <- 
    {tile_type = Wall (Corner (Left, Top)); bottom_left = (corner_x, top_y)};
   tile_array.(0).(x_dim - 1) <- 
    {tile_type = Wall (Corner (Right, Bottom)); 
     bottom_left = (right_x, corner_y)};
   tile_array.(y_dim - 1).(x_dim - 1) <- 
    {tile_type = Wall (Corner (Right, Top)); bottom_left = (right_x, top_y)};
   ()

   let make_shell (x_dim: int) (y_dim: int) (map_corner: point) 
    (tile_array: map_tile array array) : unit =
   make_corners x_dim y_dim map_corner tile_array;

   () *)

let standard_maze = 
  [|[|Wall (Corner (Bottom, Left)); Wall Vertical; Wall Vertical; Wall Vertical;
      Wall Vertical; Wall Vertical; Wall Vertical; Wall Vertical; Wall Vertical;
      Wall Vertical; Wall(Corner (Top, Left))|];
    [|Wall Horizontal; Food; Food; Food; Food; Food; Food; Food; Food; Food; 
      Wall Horizontal|];
    [|Wall Horizontal; Food; Wall (Corner (Bottom, Left)); Wall Vertical; 
      Wall (End Top); Food; Wall (End Bottom); Wall Vertical; 
      Wall (Corner (Top, Left)); Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Wall (End Right); Food; Food; Food; Food; Food; 
      Wall (End Right); Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Food; Food; Wall (Corner (Bottom, Left)); 
      Wall Vertical; Wall (Corner (Top, Left)); Food; Food; Food; 
      Wall Horizontal|];
    [|Wall Horizontal; Food; Wall (End Left); Food; Wall Horizontal; Food;
      Wall Horizontal; Food; Wall (End Left); Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Wall Horizontal; Food; Wall (End Right); Food; 
      Wall (End Right); Food; Wall Horizontal; Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Wall (End Right); Food; Food; Food; Food; Food;
      Wall (End Right); Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Food; Food; Wall (End Bottom); Wall Vertical; Wall
        (End Top); Food; Food; Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Wall (End Left); Food; Food; Food; Food; Food; Wall 
        (End Left); Food; Wall Horizontal|];    
    [|Wall Horizontal; Food; Wall Horizontal; Food; Wall (Corner (Bottom, Left))
     ; Wall Vertical; Wall (Corner (Top, Left)); Food; Wall Horizontal; Food;
      Wall Horizontal|];
    [|Wall Horizontal; Food; Wall Horizontal; Food; Wall Horizontal; Empty; 
      Wall (End Right); Food; Wall Horizontal; Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Wall Horizontal; Food; Wall Horizontal; Empty;
      Empty; Food; Wall Horizontal; Food; Wall Horizontal;|];
    [|Wall Horizontal; Food; Wall Horizontal; Food; Wall Horizontal; Empty;
      Empty; Food; Wall Horizontal; Food; Wall Horizontal;|];
    [|Wall Horizontal; Food; Wall Horizontal; Food; Wall Horizontal; Empty; 
      Wall (End Left); Food; Wall Horizontal; Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Wall Horizontal; Food; 
      Wall (Corner (Bottom, Right)); Wall Vertical; Wall (Corner (Top, Right)); 
      Food; Wall Horizontal; Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Wall (End Right); Food; Food; Food; Food; Food; 
      Wall (End Right); Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Food; Food; Wall (End Bottom); Wall Vertical; Wall
        (End Top); Food; Food; Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Wall (End Left); Food; Food; Food; Food; Food;
      Wall (End Left); Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Wall Horizontal; Food; Wall (End Left); Food; 
      Wall (End Left); Food; Wall Horizontal; Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Wall (End Right); Food; Wall Horizontal; Food;
      Wall Horizontal; Food; Wall (End Right); Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Food; Food; Wall (Corner (Bottom, Right)); 
      Wall Vertical; Wall (Corner (Top, Right)); Food; Food; Food; 
      Wall Horizontal|];
    [|Wall Horizontal; Food; Wall (End Left); Food; Food; Food; Food; Food; 
      Wall (End Left); Food; Wall Horizontal|];
    [|Wall Horizontal; Food; Wall (Corner (Bottom, Right)); Wall Vertical; 
      Wall (End Top); Food; Wall (End Bottom); Wall Vertical; 
      Wall (Corner (Top, Right)); Food; Wall Horizontal|];  
    [|Wall Horizontal; Food; Food; Food; Food; Food; Food; Food; Food; Food; 
      Wall Horizontal|];
    [|Wall (Corner (Bottom, Right)); Wall Vertical; Wall Vertical; 
      Wall Vertical; Wall Vertical; Wall Vertical; Wall Vertical; Wall Vertical;
      Wall Vertical;Wall Vertical; Wall(Corner (Top, Right))|];
  |]

let make_tile_types (x_dim: int) (y_dim: int) : tile array array = 
  let tile_types = Array.make_matrix y_dim x_dim Food in 
  for i = 0 to 25 do 
    for j = 0 to x_dim - 1 do 
      tile_types.(i).(j) <- standard_maze.(i).(j);
    done;
  done;
  tile_types

let make_tile (x: int) (y: int) (map_corner: point) (tile_type: tile) :
  map_tile = 
  let map_x = fst map_corner in 
  let map_y = snd map_corner in
  let position = coordinate_to_position (x, y) map_corner in 
  let shifted_position = (fst position + map_x, snd position + map_y) in
  {
    tile_type = tile_type;
    bottom_left = shifted_position;
  }

let make_tiles (x_dim: int) (y_dim: int) (map_corner: point) 
    (tile_types : tile array array): map_tile array array = 
  let default_tile = 
    {
      tile_type = Empty;
      bottom_left = map_corner;
    }
  in
  (* let top_left = (fst map_corner, snd map_corner + y_dim * tile_size) in  *)
  let tile_array = Array.make_matrix y_dim x_dim default_tile in
  (* make_shell x_dim y_dim map_corner tile_array; *)
  for i = 0 to y_dim - 1 do 
    for j = 0 to x_dim - 1 do 
      tile_array.(i).(j) <- make_tile i j map_corner tile_types.(i).(j)
    done;
  done;
  tile_array

let make_map (width: int) (height: int) (corner: point): t =   
  let tiles_x = width / tile_size in
  let tiles_y = height / tile_size in  
  let tile_types = make_tile_types tiles_y tiles_x in 
  let tile_list = make_tiles tiles_y tiles_x corner tile_types in 
  {tiles = tile_list; width = width; height = height; bottom_left = corner}

let update_map (map: t) (pos: int * int) : t = 
  failwith "unimplemented"

let draw_corner_single (first: point) (second: point) : unit = 
  let fst_x = fst first in 
  let fst_y = snd first in 
  let snd_x = fst second in 
  let snd_y = snd second in 
  moveto fst_x fst_y;
  lineto fst_x snd_y;
  lineto snd_x snd_y

let draw_corner_double (first: point) (second: point) (corner_type: corner) :
  unit = 
  draw_corner_single first second;
  let fst_x = fst first in 
  let fst_y = snd first in 
  let snd_x = fst second in 
  let snd_y = snd second in 
  let first_x = if snd corner_type = Left then fst_x + wall_width 
    else fst_x - wall_width in 
  let second_y = if fst corner_type = Top then snd_y - wall_width 
    else snd_y + wall_width in 
  draw_corner_single (first_x, fst_y) (snd_x, second_y)


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


let draw_wall_lines (first: point) (second: point) : unit = 
  let fst_x = fst first in 
  let fst_y = snd first in 
  let snd_x = fst second in 
  let snd_y = snd second in 

  moveto fst_x fst_y;
  let endpoint_x = if fst_x = snd_x then fst_x + tile_size else fst_x in 
  let endpoint_y = if fst_y = snd_y then fst_y + tile_size  else fst_y in 
  lineto endpoint_x endpoint_y;

  moveto snd_x snd_y;
  let endpoint_x = if fst_x = snd_x then snd_x + tile_size else snd_x  in 
  let endpoint_y = if fst_y = snd_y then snd_y + tile_size else snd_y in 
  lineto endpoint_x endpoint_y

let draw_wall_cap (tile_x: int) (tile_y: int) (dir: orientation) : unit = 
  let margin = (tile_size - wall_width) / 2 in 
  let fst = margin in 
  let snd = tile_size - margin in 
  match dir with 
  | Top -> 
    let y_pos = tile_y + tile_size in
    moveto (tile_x + fst) y_pos;
    lineto (tile_x + snd) y_pos
  | Bottom -> 
    moveto (tile_x + fst) tile_y;
    lineto (tile_x + snd) tile_y;
  | Left -> 
    moveto tile_x (tile_y + fst);
    lineto tile_x (tile_y + snd);
  | Right -> 
    let x_pos = tile_x + tile_size in 
    moveto x_pos (tile_y + fst);
    lineto x_pos (tile_y + snd)

let draw_wall_end (tile: map_tile) (end_type: orientation) : unit = 
  let margin = (tile_size - wall_width) / 2 in 
  let tile_corner = tile.bottom_left in 
  let tile_x = fst tile_corner in 
  let tile_y = snd tile_corner in 
  let first = margin in 
  let second = tile_size - margin in 
  match end_type with 
  | Top | Bottom as dir -> 
    draw_wall_lines (tile_x + first, tile_y) (tile_x + second, tile_y);
    draw_wall_cap tile_x tile_y dir

  | Left | Right as dir -> 
    draw_wall_lines (tile_x, tile_y + first) (tile_x, tile_y + second);
    draw_wall_cap tile_x tile_y dir

let draw_wall_normal (tile: map_tile) (orientation: wall) : unit = 
  let margin = (tile_size - wall_width) / 2 in 
  let tile_corner = tile.bottom_left in 
  let tile_x = fst tile_corner in 
  let tile_y = snd tile_corner in 
  let first = margin in 
  let second = tile_size - margin in 
  match orientation with 
  | Vertical -> 
    draw_wall_lines (tile_x + first, tile_y) (tile_x + second, tile_y)

  | Horizontal -> 
    draw_wall_lines (tile_x, tile_y + first) (tile_x, tile_y + second)

  | _ -> ()

let draw_wall (tile: map_tile) (wall_type: wall) : unit = 
  set_color wall_color;
  match wall_type with 
  | Vertical -> draw_wall_normal tile wall_type
  | Horizontal -> draw_wall_normal tile wall_type
  | Corner corner -> draw_corner_full tile corner
  | End endwall -> draw_wall_end tile endwall

let draw_food (tile: map_tile) (radius: int) : unit = 
  set_color food_color;
  let tile_corner = tile.bottom_left in 
  let x_center = fst tile_corner + tile_size / 2 in 
  let y_center = snd tile_corner + tile_size / 2 in 
  fill_circle x_center y_center radius

(* temporary helper function to make map tile outlines visible *)
let draw_outline (tile: map_tile) : unit = 
  set_color wall_color;
  let bottom_corner = tile.bottom_left in 
  let x_position = fst bottom_corner in 
  let y_position = snd bottom_corner in 
  draw_rect x_position y_position tile_size tile_size

let draw_tile (tile: map_tile) : unit =
  (* draw_outline tile;  *)
  match tile.tile_type with
  | Empty -> ()
  | Food -> draw_food tile food_radius
  | Special -> draw_food tile special_radius
  | Wall wall -> draw_wall tile wall

let draw_map_row (map_row : map_tile array) : unit = 
  ignore (Array.map draw_tile map_row);
  ()

let draw_map (map: t) : unit = 
  ignore (Array.map draw_map_row map.tiles);
  ()

let map (map: t) : map_tile array array = map.tiles

(* let make_map_single (width: int) (height: int) (corner: point): t = 
   let tile = 
    {
      tile_type = Wall (Corner (Right, Top));
      bottom_left = corner
    }
   in 
   let tile_list = Array.make_matrix 1 1 tile in 
   {tiles = tile_list; width = width; height = height; bottom_left = corner}

   (* temporary helper *)
   let draw_tile_helper (map: t) : unit = 
   let corner = map.bottom_left in 
   draw_rect (fst corner) (snd corner) tile_size tile_size;
   let tiles = map.tiles in 
   let tile = tiles.(0).(0) in 
   draw_tile tile *)




