open Sprite 
open Constants

exception UnknownDirection 

type direction = Right | Left | Up | Down

type state = Active | Scared1 | Scared2 | Eaten

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
  mutable state: state
}
(* constants for ghosts *)
let ghost_width = 50
let ghost_height = 50

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

let scared1_coordinates = [(8, 4); (9, 4)]

let scared2_coordinates = [(10, 4); (11, 4)]

let eaten_coordinates = [(8, 5); (9, 5); (10, 5); (11, 5)]

let map_sprites (shift: int) ((x, y): int * int) : Sprite.t = 
  sprite_from_sheet sprite_sheet x y ghost_width ghost_height shift

let scared1_sprites : Sprite.t list =  
  let shift = 4 in
  List.map (map_sprites shift) scared1_coordinates 

let scared2_sprites : Sprite.t list = 
  let shift = 4 in
  List.map (map_sprites shift) scared2_coordinates 

let eaten_sprites: ghost_sprites = 
  let shift = 4 in 
  let right = [map_sprites shift (List.nth eaten_coordinates 0)] in 
  let left = [map_sprites shift (List.nth eaten_coordinates 1)] in 
  let up = [map_sprites shift (List.nth eaten_coordinates 2)] in 
  let down = [map_sprites shift (List.nth eaten_coordinates 3)] in 
  {right = right; left = left; up = up; down = down} 

let make_ghost_sprite coordinates : ghost_sprites = 
  let shift = 5 in
  let right = List.map (map_sprites shift) (List.nth coordinates 0) in 
  let left = List.map (map_sprites shift) (List.nth coordinates 1) in 
  let up = List.map (map_sprites shift) (List.nth coordinates 2) in 
  let down = List.map (map_sprites shift) (List.nth coordinates 3) in
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
  state = Active;
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
    else begin 
      let modulo = if g.state = Eaten then 1 else 2 in
      (g.move_counter + 1) mod modulo
    end
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

let get_sprite_dir (g: t) (sprites: ghost_sprites) = 
  let sprite_list = 
    match g.direction with
    | Right -> sprites.right 
    | Left -> sprites.left 
    | Up -> sprites.up
    | Down -> sprites.right 
  in 
  List.nth sprite_list g.move_counter

let get_sprite g = 
  match g.state with 
  | Active -> get_sprite_dir g g.sprites
  | Scared1 -> List.nth scared1_sprites g.move_counter 
  | Scared2 -> List.nth scared2_sprites g.move_counter 
  | Eaten -> get_sprite_dir g eaten_sprites
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

let get_state (ghost: t) : string = 
  match ghost.state with 
  | Active -> "active"
  | Scared1 -> "scared1"
  | Scared2 -> "scared2"
  | Eaten -> "eaten"

let set_state (ghost: t) (state: string) : unit = 
  let state = 
    match state with 
    | "active" -> Active
    | "scared1" -> Scared1
    | "scared2" -> Scared2 
    | "eaten" -> Eaten 
    | _ -> failwith "Error: not a valid ghost state!"
  in
  if state = Eaten then ghost.move_counter <- 0;
  ghost.state <- state