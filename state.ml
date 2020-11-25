open Ghost
open Map
type t = 
  {
    points:int;
    lives:int;
    ghosts: Ghost.t array;
    current_level: int;
    map: Map.t;
  }

let points state =
  state.points

let lives state =
  state.lives

let ghosts state =
  state.ghosts

let current_level state =
  state.current_level

let initial_state map ghosts_entry=
  {points = 5;
   lives = 3;
   ghosts = ghosts_entry;
   current_level = 1;
   map = map} 

let update_state_food state map =
  {points = state.points + 1;
   lives = 3;
   ghosts = state.ghosts;
   current_level = 1;
   map = map}

(*let make_ghosts num min_x min_y = 
  let rec set_ghosts_helper acc counter = function 
    | n when n>0 -> set_ghosts_helper 
                      ((new_g (min_x + (50 * counter)) min_y)::acc )
                      (counter + 1) (n - 1)
    | _ -> acc |> List.rev |> Array.of_list 
  in 
  set_ghosts_helper [] 0 num*)