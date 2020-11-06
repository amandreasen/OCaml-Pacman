open Images

type t = {
  mutable x : int;
  mutable y : int;
}

let new_player = {x = 0; y = 0}

let get_position player = 
  (player.x, player.y)

let move (player : t) (dir : int * int) = 
  player.x <- fst (get_position player) + fst dir; 
  player.y <- snd (get_position player) + snd dir 