type t = {
  mutable x : int;
  mutable y : int;
  mutable is_following : bool; 
  mutable following_counter : int;
  mutable prev_move : int *int 
}

let new_ghost x_pos y_pos init_move = {
  x = x_pos; 
  y = y_pos; 
  is_following = false; 
  following_counter = 0;
  prev_move = init_move
}

let get_position g =
  (g.x, g.y)

let prev_move g = 
  g.prev_move

let move (g : t) (dir : int * int) = 
  g.x <- g.x + fst dir; 
  g.y <- g.y + snd dir; 
  g.prev_move <- dir

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
