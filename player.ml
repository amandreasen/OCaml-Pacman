open Sprite

type direction = Up | Right | Down | Left

type t = {
  mutable x : int;
  mutable y : int;
  mutable direction : direction; 
  image : Sprite.t;
  mutable prev_move : int * int;
  mutable prev_move_attempt : int * int 
}


(**let new_player = {image = Sprite.make_sprite "pacman.png"; x = 0; y = 0}*)
let new_player = 
  {
    x = 175; 
    y = 175;
    direction = Right;
    image = Sprite.make_sprite "pacman2.png";
    prev_move = (0,0);
    prev_move_attempt = (0,0)
  }

let get_position player = 
  (player.x, player.y)

let move (player : t) (dir : int * int) = 
  player.x <- fst (get_position player) + fst dir; 
  player.y <- snd (get_position player) + snd dir 

let player_direction player = 
  player.direction

let player_image user = 
  user.image

let player_prev_move user = 
  user.prev_move

let player_prev_attempt user = 
  user.prev_move_attempt

let move_made user move = 
  user.prev_move <- move

let move_attempt user move = 
  user.prev_move_attempt <- move

