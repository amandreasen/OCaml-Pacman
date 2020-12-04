open Sprite

type direction = Up | Right | Down | Left

(* constants for player *)
let player_width = 50
let player_height = 50
let init_dir = Right 
let init_pos = (175, 175)

(* sprite sheet coordinates *)
let player_init = (2, 0)
let player_right = [(0, 0); (1, 0)]
let player_left = [(1, 0); (1, 1)]
let player_down = [(0, 3); (1, 3)]
let player_up = [(0, 2); (1, 2)]
let player_death = [(0, 3); (0, 4); (0, 5); (0, 6); (0, 7); (0, 8); (0, 9);
                    (0, 10); (0, 11); (0, 12); (0, 13)]

(* load sprites *)
let sprite_sheet = Png.load_as_rgb24 ("./sprites/sprite_sheet.png") []

type player_sprites = {
  init : Sprite.t;
  right : Sprite.t list;
  left : Sprite.t list;
  down : Sprite.t list;
  up : Sprite.t list;
  death : Sprite.t list;
}

type t = {
  mutable x : int;
  mutable y : int;
  mutable direction : direction; 
  image : Sprite.t;
  mutable prev_move : int * int;
  mutable prev_move_attempt : int * int 
}

let new_player = 
  {
    x = fst init_pos; 
    y = snd init_pos;
    direction = init_dir;
    image = Sprite.make_sprite "pacman2.png";
    prev_move = (0,0);
    prev_move_attempt = (0,0)
  }

let get_position player = 
  (player.x, player.y)

let move (player : t) (dir : int * int) = 
  let update_dir = 
    match dir with 
    | (x,0) when x > 0 -> Right
    | (x,0) when x < 0 -> Left
    | (0,y) when y > 0 -> Up
    | (0,y) when y < 0 -> Down
    | _ -> player.direction
  in 
  player.direction <- update_dir; 
  player.x <- fst (get_position player) + fst dir; 
  player.y <- snd (get_position player) + snd dir;
  player.prev_move <- dir

let player_direction player = 
  player.direction

let player_image user = 
  user.image
[@@coverage off]

let player_prev_move user = 
  user.prev_move

let player_prev_attempt user = 
  user.prev_move_attempt

let move_attempt user move = 
  user.prev_move_attempt <- move