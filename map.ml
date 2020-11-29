open Graphics

(* Constants *)
let tile_size = 50
let wall_width = 20
let food_radius = 3
let special_radius = 6
let food_color = rgb 255 184 245
let wall_color = Graphics.blue
let pacman_rad = 25

type point = int * int

type coordinate = int * int

type orientation = Top | Bot | Left | Right

(* (y,x) where x is horizontal orientation and y is vertical orientation *)
type corner = orientation * orientation 

type wall = Vert | Horz | Corner of corner | End of orientation

type tile = Empty | Food | Special | Ghost | Wall of wall

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

let standard_map = 
  {
    tiles = [||];
    width = 1300;
    height = 550;
    bottom_left = (0,0)
  } 

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
(* 
let check_food pos map =
  let (acc:int ref) = ref 0 in 
  for i = 0 to (Array.length map.tiles) - 1 do
    let col = map.tiles.(i) in
    for j = 0 to (Array.length col) - 1 do
      let tile = col.(j) in
      if (check_contains pos tile.bottom_left) then 
        begin
          if (!acc = 0) then
            match tile.tile_type with
            | Food  -> 
              let new_tile = {tile with tile_type = Empty} in
              (acc := !acc + 1);
              col.(j) <- new_tile; 
            | _ -> ();
        end
    done;
  done *)

let map_shift = 100

let position_to_coordinate (position: point) : coordinate = 
  let x_position = fst position - map_shift in 
  let y_position = snd position - map_shift in 
  let x_coordinate = x_position / tile_size in 
  let y_coordinate = y_position / tile_size in 
  (x_coordinate, y_coordinate)

let coordinate_to_position (coordinate: coordinate) (map_corner: point) : 
  point = 
  let x_coordinate = fst coordinate in 
  let y_coordinate = snd coordinate in 
  let map_x = fst map_corner in 
  let map_y = snd map_corner in 
  let x_position = x_coordinate * tile_size + map_x in 
  let y_position = y_coordinate * tile_size + map_y in 
  (x_position, y_position)

let check_food pos map =
  let coordinate = position_to_coordinate pos in 
  let x = fst coordinate in 
  let y = snd coordinate in 
  let tile = map.tiles.(x).(y) in 
  match tile.tile_type with 
  | Food -> 
    map.tiles.(x).(y) <- {tile with tile_type = Empty} 
  | _ -> ()

let make_tile (x: int) (y: int) (map_corner: point) (tile_type: tile) :
  map_tile = 
  let position = coordinate_to_position (x, y) map_corner in 
  {
    tile_type = tile_type;
    bottom_left = position;
  }

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

let make_map (corner: point) (maze_name: string)
  : t =   
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

let draw_wall_cap (tile_x: int) (tile_y: int) (dir: orientation) : unit = 
  let margin = (tile_size - wall_width) / 2 in 
  let fst = margin in 
  let snd = tile_size - margin in 
  match dir with 
  | Top -> 
    let y_pos = tile_y + tile_size in
    moveto (tile_x + fst) y_pos;
    lineto (tile_x + snd) y_pos
  | Bot -> 
    moveto (tile_x + fst) tile_y;
    lineto (tile_x + snd) tile_y;
  | Left -> 
    moveto tile_x (tile_y + fst);
    lineto tile_x (tile_y + snd);
  | Right -> 
    let x_pos = tile_x + tile_size in 
    moveto x_pos (tile_y + fst);
    lineto x_pos (tile_y + snd)

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

(* have helper functions that calculate y coordinate and x coordinate based 
   on direction -- draw wall lines draws from left ro right, or bottom to top*)

let draw_wall_end (tile: map_tile) (end_type: orientation) : unit = 
  let margin = (tile_size - wall_width) / 2 in 
  let tile_corner = tile.bottom_left in 
  let tile_x = fst tile_corner in 
  let tile_y = snd tile_corner in 
  let shift = 3 * tile_size / 4 in
  let coordinates = calculate_coordinates end_type margin tile_x tile_y shift in 
  let pos_one = fst coordinates in 
  let pos_two = snd coordinates in 
  draw_wall_lines pos_one pos_two shift
(* match end_type with 
   | Top | Bot as dir -> 
   let fst = (tile_x + first, tile_y) in 
   let snd = (tile_x + second, tile_y) in
   draw_wall_lines fst snd shift;
   draw_wall_cap tile_x tile_y dir

   | Left | Right as dir -> 
   let fst = (tile_x, tile_y + first) in 
   let snd = (tile_x, tile_y + second) in
   draw_wall_lines fst snd shift;
   draw_wall_cap tile_x tile_y dir *)

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

let draw_wall (tile: map_tile) (wall_type: wall) : unit = 
  set_color wall_color;
  match wall_type with 
  | Vert -> draw_wall_normal tile wall_type
  | Horz -> draw_wall_normal tile wall_type
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
  | Empty | Food | Ghost -> ()
  | Special -> draw_food tile special_radius
  | Wall wall -> draw_wall tile wall

let draw_map_row (map_row : map_tile array) : unit = 
  ignore (Array.map draw_tile map_row);
  ()

let draw_map (map: t) : unit = 
  ignore (Array.map draw_map_row map.tiles);
  ()

let draw_food_helper (tile: map_tile) : unit = 
  match tile.tile_type with 
  | Food -> draw_food tile food_radius
  | _ -> ()

let draw_food_row (food_row: map_tile array) : unit = 
  ignore (Array.map draw_food_helper food_row);
  ()

let draw_food (map: t) : unit =
  ignore (Array.map draw_food_row map.tiles);
  ()



