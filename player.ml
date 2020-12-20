open Sprite
open Constants

type direction = Up | Right | Down | Left

(* constants for player *)
let init_dir = Right 

(* sprite sheet coordinates *)
let player_right = [(2, 0); (1, 0); (0, 0)]
let player_left = [(2, 0); (1, 1); (0, 1)]
let player_down = [(2, 0); (1, 3); (0, 3)]
let player_up = [(2, 0); (1, 2); (0, 2)]
let player_death = [(2, 0); (3, 0); (4, 0); (5, 0); (6, 0); (7, 0); (8, 0); 
                    (9, 0); (10, 0); (11, 0); (12, 0); (13, 0)]

type player_sprites = {
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
  mutable move_counter : int;
  images : player_sprites;
  mutable prev_move : int * int;
  mutable prev_move_attempt : int * int;
  dying: bool
}

let make_images : player_sprites = 
  let shift = 2 in
  let map_sprites (x, y) = 
    sprite_from_sheet sprite_sheet x y player_width player_height shift
  in
  let right = List.map map_sprites player_right in 
  let left = List.map map_sprites player_left in 
  let down = List.map map_sprites player_down in 
  let up = List.map map_sprites player_up in 
  let death = List.map map_sprites player_death in 
  {right = right; left = left; down = down; up = up; death = death}
[@@coverage off]

let new_player = 
  fun () ->
  {
    x = fst init_pos; 
    y = snd init_pos;
    direction = init_dir;
    move_counter = 0;
    images = make_images;
    prev_move = (0,0);
    prev_move_attempt = (0,0);
    dying = false;
  }

let get_position player = 
  (player.x, player.y)

(** [helper_new_dir player dir] is the new direction that [player] should look 
    in after making a move by [dir]. *)
let helper_new_dir player = function 
  | (x,0) when x > 0 -> Right
  | (x,0) when x < 0 -> Left
  | (0,y) when y > 0 -> Up
  | (0,y) when y < 0 -> Down
  | _ -> player.direction

let move (player : t) (dir : int * int) = 
  let update_dir = helper_new_dir player dir in 
  let counter = 
    if player.direction <> update_dir 
    then 0 
    else begin 
      if dir <> (0,0) || (dir = (0, 0) && player.move_counter <> 1)
      then (player.move_counter + 1) mod 3
      else player.move_counter
    end 
  in 
  player.move_counter <- counter;
  player.direction <- update_dir; 
  player.x <- player.x + fst dir; 
  player.y <- player.y + snd dir;
  player.prev_move <- dir

let player_image (user : t) = 
  let images = user.images in 
  let sprite_list = 
    if user.dying then images.death 
    else match user.direction with 
      | Right -> images.right 
      | Left -> images.left
      | Up -> images.up 
      | Down -> images.down 
  in 
  List.nth sprite_list user.move_counter
[@@coverage off]

let player_prev_move user = 
  user.prev_move

let player_prev_attempt user = 
  user.prev_move_attempt

let move_attempt user move = 
  if move <> (0, 0) then user.prev_move_attempt <- move else ()

let start_death user = 
  {user with move_counter = 0; dying = true;}

let reset_move user = 
  user.move_counter <- 0

let animate_death user : unit =  
  Unix.sleepf(0.02);
  let move = user.move_counter in 
  if move < 12 then user.move_counter <- move + 1 else ()

let death_ended user : bool = 
  user.dying && user.move_counter = 11
