open Sprite 

exception UnknownDirection 

type direction = Right | Left | Up | Down

type ghost_sprites = {
  right: Sprite.t list;
  left: Sprite.t list;
  up: Sprite.t list;
  down: Sprite.t list;
}

type t = {
  mutable x : int;
  mutable y : int;
  mutable is_following : bool; 
  mutable following_counter : int;
  mutable prev_move : int * int;
  mutable move_counter : int;
  sprites : ghost_sprites;
  mutable direction: direction
}
(* constants for ghosts *)
let ghost_width = 50
let ghost_height = 50

let sprite_sheet = 
  let sheet = Png.load_as_rgb24 ("./sprites/sprite_sheet.png") [] in 
  let black_box = Png.load_as_rgb24 ("./sprites/black.png") [] in 
  Images.blit black_box 0 0 sheet 100 45 350 100;
  sheet

let red_coordinates = [[(0, 4); (1, 4)]; [(2, 4); (3, 4)]; [(4, 4); (5, 4)];
                       [(6, 4); (7, 4)]]

(* right left up down *)
let make_ghost_sprite coordinates : ghost_sprites = 
  let map_sprites (x, y) = 
    sprite_from_sheet sprite_sheet x y ghost_width ghost_height 
  in
  let right = List.map map_sprites (List.nth coordinates 0) in 
  let left = List.map map_sprites (List.nth coordinates 1) in 
  let up = List.map map_sprites (List.nth coordinates 2) in 
  let down = List.map map_sprites (List.nth coordinates 3) in
  {right = right; left = left; up = up; down = down} 

let make_sprites (color: string) : ghost_sprites =
  match color with 
  | "red" -> make_ghost_sprite red_coordinates 
  | "pink" -> failwith "unimplemented"
  | "cyan" -> failwith "unimplemented"
  | "orange" -> failwith "unimplemented"
  | _ -> failwith "invalid ghost color"

let new_ghost x_pos y_pos init_move color = {
  x = x_pos; 
  y = y_pos; 
  is_following = false; 
  following_counter = 0;
  prev_move = init_move;
  move_counter = 0;
  sprites = make_sprites color;
  direction = Right
}

let get_position g =
  (g.x, g.y)

let prev_move g = 
  g.prev_move

let move (g : t) (dir : int * int) = 
  g.x <- g.x + fst dir; 
  g.y <- g.y + snd dir; 
  let direction =
    match dir with 
    | (x, 0) when x > 0 -> Right 
    | (x, 0) when x < 0 -> Left 
    | (0, y) when y > 0 -> Up 
    | (0, y) when y < 0 -> Down 
    | _ -> g.direction 
  in 
  let counter = 
    if g.direction <> direction then 0 
    else (g.move_counter + 1) mod 2 
  in 
  g.prev_move <- dir;
  g.direction <- direction;
  g.move_counter <- counter

let is_following g = 
  g.is_following

let following_counter g = 
  g.following_counter

let incr_following_count g = 
  g.following_counter <- g.following_counter + 1

let reset_following g = 
  g.is_following <- false;
  g.following_counter <- 0

let start_following g =  
  g.is_following <- true;
  g.following_counter <- 1

let get_sprite g = 
  let sprites = g.sprites in 
  let sprite_list = 
    match g.direction with
    | Right -> sprites.right 
    | Left -> sprites.left 
    | Up -> sprites.up
    | Down -> sprites.right 
  in 
  List.nth sprite_list g.move_counter
[@@coverage off]

