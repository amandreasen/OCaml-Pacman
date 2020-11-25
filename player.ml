open Sprite

type t = {
  mutable x : int;
  mutable y : int;
  (* image : Sprite.t; *)
}


(**let new_player = {image = Sprite.make_sprite "pacman.png"; x = 0; y = 0}*)
let new_player = 
  {x = 175; 
   y = 175;}
(* image = Sprite.make_sprite "pacman.png"} *)

let get_position player = 
  (player.x, player.y)

let move (player : t) (dir : int * int) = 
  player.x <- fst (get_position player) + fst dir; 
  player.y <- snd (get_position player) + snd dir 


let player_image user = 
  (* user.image *)
  failwith "unimplemented"
(* *  display_mode true; *)
(* ignore (user.image); *)




