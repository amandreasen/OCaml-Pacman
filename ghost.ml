type t = {
  mutable x : int;
  mutable y : int;
  mutable is_following : bool; 
  mutable following_counter : int;
}

let new_g x_pos y_pos = {
  x = x_pos; 
  y = y_pos; 
  is_following = false; 
  following_counter = 0
}

let get_pos g =
  (g.x, g.y)

let move (ghost : t) (dir : int * int) = 
  ghost.x <- ghost.x + fst dir; 
  ghost.y <- ghost.y + snd dir 

let is_following g = 
  g.is_following

let following_counter g = 
  g.following_counter