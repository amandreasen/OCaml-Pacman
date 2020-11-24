type t = {
  mutable x : int;
  mutable y : int;
}

let new_g x_pos y_pos = {x = x_pos; y = y_pos}

let get_pos g =
  (g.x, g.y)

let move (ghost : t) (dir : int * int) = 
  ghost.x <- ghost.x + fst dir; 
  ghost.y <- ghost.y + snd dir 

let rand_dir = 
  match Random.int 4 with 
  | 0 -> (50,0)
  | 1 -> (0,50)
  | 2 -> (-50,0)
  | 3 -> (0,-50)
  | _ -> (0,0)
