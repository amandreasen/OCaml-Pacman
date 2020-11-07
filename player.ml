open Graphics
open Images
open Sprite

type t = {
  image : Sprite.t;
  mutable x : int;
  mutable y : int;
}

let new_player = {image = Sprite.make_sprite "pacman.png"; x = 0; y = 0}

let get_position player = 
  (player.x, player.y)

let move (player : t) (dir : int * int) = 
  player.x <- fst (get_position player) + fst dir; 
  player.y <- snd (get_position player) + snd dir 

let draw_player user = 
  display_mode true;
  ignore (user.image);
  ()
