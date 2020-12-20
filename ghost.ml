open Sprite 
open Constants

(**[direction] represents a direction of movement. *)  
type direction = Right | Left | Up | Down

(**[state] represents a ghost state. [Active] ghosts are their normal color 
   and are able to eat the player. [Scared1] ghosts are blue and are able to be 
   eaten by the player. [Scared2] ghosts are white and are able to be eaten by 
   the player. [Eaten] ghosts are only visible as a pair of eyes and have 
   already been eaten by the player (and cannot be eaten again while in this
   state).*) 
type state = Active | Scared1 | Scared2 | Eaten

(**[ghost_sprites] contains a list of ghost sprites for the right, left, up, 
   and down movement directions. *) 
type ghost_sprites = {
  right: Sprite.t list;
  left: Sprite.t list;
  up: Sprite.t list;
  down: Sprite.t list;
}

(**Type [t] represents a ghost in the map. The type contains information about 
   the ghost's pixel position, movement and player following history, animation
   sprites, and state in the game. *) 
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
  mutable state: state;
  mutable eaten_timer: int;
  mutable eaten: bool;
}

(* Sprite sheet coordinates for the ghosts of the form [right, left, up 
   down], grouped by ghost color.*)

(* Sprite sheet coordinates for the red ghost. *)
let red_coordinates = 
  [[(0, 4); (1, 4)]; [(2, 4); (3, 4)]; [(4, 4); (5, 4)]; [(6, 4); (7, 4)]]

(* Sprite sheet coordinates for the pink ghost. *)
let pink_coordinates = 
  [[(0, 5); (1, 5)]; [(2, 5); (3, 5)]; [(4, 5); (5, 5)]; [(6, 5); (7, 5)]]

(* Sprite sheet coordinates for the cyan ghost. *)
let cyan_coordinates = 
  [[(0, 6); (1, 6)]; [(2, 6); (3, 6)]; [(4, 6); (5, 6)]; [(6, 6); (7, 6)]]

(* Sprite sheet coordinates for the orange ghost. *)
let orange_coordinates = 
  [[(0, 7); (1, 7)]; [(2, 7); (3, 7)]; [(4, 7); (5, 7)]; [(6, 7); (7, 7)]]

(* Sprite sheet coordinates for scared blue ghost sprites. *)
let scared1_coordinates = [(8, 4); (9, 4)]

(* Sprite sheet coordinates for scared white ghost sprites. *)
let scared2_coordinates = [(10, 4); (11, 4)]

(* Sprite sheet coordinates for eaten ghost sprites. *)
let eaten_coordinates = [(8, 5); (9, 5); (10, 5); (11, 5)]

(**[map_sprites shift (x, y)] will return a sprite loaded from the standard
   sprite sheet with sprite sheet coordinate (x, y). The coordinate will be 
   calculated after shifting over [shift] pixels. *)  
let map_sprites (shift: int) ((x, y): int * int) : Sprite.t = 
  sprite_from_sheet sprite_sheet x y ghost_width ghost_height shift

(**[scared1_sprites] is a list of sprites for blue scared ghosts. *)  
let scared1_sprites : Sprite.t list =  
  let shift = 4 in
  List.map (map_sprites shift) scared1_coordinates 

(**[scared2_sprites] is a list of sprites for white scared ghosts. *)  
let scared2_sprites : Sprite.t list = 
  let shift = 3 in
  List.map (map_sprites shift) scared2_coordinates 

(**[eaten sprites] is a list of sprites for eaten ghosts, grouped by 
   movement direction.*)  
let eaten_sprites: ghost_sprites = 
  let shift = 4 in 
  let right = [map_sprites shift (List.nth eaten_coordinates 0)] in 
  let left = [map_sprites shift (List.nth eaten_coordinates 1)] in 
  let up = [map_sprites shift (List.nth eaten_coordinates 2)] in 
  let down = [map_sprites shift (List.nth eaten_coordinates 3)] in 
  {right = right; left = left; up = up; down = down} 

(**[make_ghost_sprite coordinates] will return a list of ghost sprites 
   grouped by direction, made from the list of sprite sheet coordinates 
   [coordinates]. *)  
let make_ghost_sprite coordinates : ghost_sprites = 
  let shift = 5 in
  let right = List.map (map_sprites shift) (List.nth coordinates 0) in 
  let left = List.map (map_sprites shift) (List.nth coordinates 1) in 
  let up = List.map (map_sprites shift) (List.nth coordinates 2) in 
  let down = List.map (map_sprites shift) (List.nth coordinates 3) in
  {right = right; left = left; up = up; down = down} 

(**[make_sprites color] will return a list of ghost_sprites grouped by 
   direction for the ghost with color [color]. 
   Requires: [color] is one of "red", "pink", "cyan", or "orange". Fails 
   otherwise. *) 
let make_sprites (color: string) : ghost_sprites =
  let coordinates  = 
    match color with 
    | "red" -> red_coordinates 
    | "pink" -> pink_coordinates
    | "cyan" -> cyan_coordinates
    | "orange" -> orange_coordinates
    | _ -> failwith "invalid ghost color"
  in 
  make_ghost_sprite coordinates

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
  eaten_timer = 0;
  eaten = false
}

let get_position g =
  (g.x, g.y)

let prev_move g = 
  g.prev_move

(**[parse_dir ghost dir] will parse the tuple direction [dir] to be one of 
   Right, Left, Up, or Down. If [dir] cannot be parsed to one of these four
   values, the current direction of [ghost] is returned instead. *) 
let parse_dir (ghost: t) (dir: int * int) : direction = 
  match dir with 
  | (x, 0) when x > 0 -> Right 
  | (x, 0) when x < 0 -> Left 
  | (0, y) when y > 0 -> Up 
  | (0, y) when y < 0 -> Down 
  | _ -> ghost.direction 

(**[update_counter ghost direction] will update the move_counter for [ghost]
   appropriately depending on the direction [direction]. If the [ghost] is
   moving in the same direction as its previous direction, then the move
   counter will be incremented. Otherwise, it will be reset to 0. *) 
let update_counter (ghost: t) (direction: direction): int = 
  if ghost.direction <> direction then 0 
  else begin 
    let modulo = if ghost.state = Eaten then 1 else 2 in
    (ghost.move_counter + 1) mod modulo
  end

(**[update_timer ghost] will update the eaten_timer of ghost [ghost]. *) 
let update_timer (ghost: t) : unit = 
  match ghost.state with 
  | Eaten -> 
    if ghost.eaten_timer = eaten_threshold then ghost.state <- Scared2;
    ghost.eaten_timer <- ghost.eaten_timer - 1
  | Scared2 -> 
    if ghost.eaten_timer = 0 && ghost.eaten 
    then begin 
      ghost.state <- Active;
      ghost.eaten <- false
    end
    else ghost.eaten_timer <- ghost.eaten_timer - 1
  | _ -> ()

let move (g : t) (dir : int * int) = 
  g.x <- g.x + fst dir; 
  g.y <- g.y + snd dir; 
  let direction = parse_dir g dir in
  let counter = update_counter g direction in
  update_timer g;
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

(** [get_sprite_dir ghost sprites] is the sprite for ghost [ghost] that is 
    from [ghost_sprites] that matches the current movement direction of the 
    ghost. *)
let get_sprite_dir (ghost: t) (sprites: ghost_sprites) = 
  let sprite_list = 
    match ghost.direction with
    | Right -> sprites.right 
    | Left -> sprites.left 
    | Up -> sprites.up
    | Down -> sprites.right 
  in 
  List.nth sprite_list ghost.move_counter

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
  if state = Eaten then begin 
    ghost.move_counter <- 0;
    ghost.eaten_timer <- eaten_time;
    ghost.eaten <- true;
  end;
  ghost.state <- state