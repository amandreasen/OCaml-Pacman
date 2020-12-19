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
  mutable direction: direction;
  mutable made_move : bool;
  mutable init_done : bool;
  mutable init_counter : int;
}
(* constants for ghosts *)
let ghost_width = 50
let ghost_height = 50

let sprite_sheet = 
  let sheet = Png.load_as_rgb24 ("./sprites/sprite_sheet.png") [] in 
  let black_box = Png.load_as_rgb24 ("./sprites/black.png") [] in 
  Images.blit black_box 0 0 sheet 100 45 350 100;
  sheet

(* sprite sheet coordinates for the ghosts of the form [right, left, up 
   down] *)
let red_coordinates = [[(0, 4); (1, 4)]; [(2, 4); (3, 4)]; [(4, 4); (5, 4)];
                       [(6, 4); (7, 4)]]

let pink_coordinates = [[(0, 5); (1, 5)]; [(2, 5); (3, 5)]; [(4, 5); (5, 5)];
                        [(6, 5); (7, 5)]]

let cyan_coordinates = [[(0, 6); (1, 6)]; [(2, 6); (3, 6)]; [(4, 6); (5, 6)];
                        [(6, 6); (7, 6)]]

let orange_coordinates = [[(0, 7); (1, 7)]; [(2, 7); (3, 7)]; [(4, 7); (5, 7)];
                          [(6, 7); (7, 7)]]

(* right left up down *)
let make_ghost_sprite coordinates : ghost_sprites = 
  let shift = 5 in
  let map_sprites (x, y) = 
    sprite_from_sheet sprite_sheet x y ghost_width ghost_height shift
  in
  let right = List.map map_sprites (List.nth coordinates 0) in 
  let left = List.map map_sprites (List.nth coordinates 1) in 
  let up = List.map map_sprites (List.nth coordinates 2) in 
  let down = List.map map_sprites (List.nth coordinates 3) in
  {right = right; left = left; up = up; down = down} 

let make_sprites (color: string) : ghost_sprites =
  match color with 
  | "red" -> make_ghost_sprite red_coordinates 
  | "pink" -> make_ghost_sprite pink_coordinates
  | "cyan" -> make_ghost_sprite cyan_coordinates
  | "orange" -> make_ghost_sprite orange_coordinates
  | _ -> failwith "invalid ghost color"

let new_ghost x_pos y_pos init_move color = {
  x = x_pos; 
  y = y_pos; 
  is_following = false; 
  following_counter = 0;
  prev_move = init_move;
  move_counter = 0;
  sprites = make_sprites color;
  direction = Right;
  made_move = true;  
  init_done = false;
  init_counter = 0;
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
  g.move_counter <- counter;
  g.made_move <- true

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

let made_move ghost = 
  ghost.made_move

let reset_move ghost = 
  ghost.made_move <- false

let is_done_initializing ghost = 
  ghost.init_done

let finish_initializing ghost = 
  ghost.init_done <- true

let move_init ghost dir = 
  ghost.init_counter <- ghost.init_counter + 1;
  move ghost dir 

let init_counter ghost = 
  ghost.init_counter

let reset_init_counter ghost = 
  ghost.init_counter <- 0