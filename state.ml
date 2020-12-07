open Ghost
open Map
open Player
open Sprite

(* constants *)
let fruit_limit = 50

type t = {
  player : Player.t;
  points : int;
  lives : int;
  ghosts : Ghost.t array;
  current_level : int;
  map : Map.t;
  follower_ghosts : Ghost.t list;
  food_left: int;
  fruit_generated: bool;
}

let player state = 
  state.player

let points state =
  state.points

let lives state =
  state.lives

let ghosts state =
  state.ghosts

let current_level state =
  state.current_level

let followers state = 
  state.follower_ghosts

let map state = 
  state.map

let initial_state player map ghosts_entry = {
  player = player;
  points = 0;
  lives = 3;
  ghosts = ghosts_entry;
  current_level = 1;
  map = map;
  follower_ghosts = [];
  food_left = food_count map;
  fruit_generated = false;
} 

let sprite_sheet = 
  let sheet = Png.load_as_rgb24 ("./sprites/sprite_sheet.png") [] in 
  let black_box = Png.load_as_rgb24 ("./sprites/black.png") [] in 
  Images.blit black_box 0 0 sheet 100 45 305 100;
  sheet

(* {
   points = state.points + 1;
   lives = 3;
   ghosts = state.ghosts;
   current_level = 1;
   map = map;
   follower_ghosts = []
   } *)
let update_state_food (state: t) (value: int) = 
  let points = state.points in
  let food_left = 
    match value with 
    | 0 -> state.food_left 
    | _ -> state.food_left - 1
  in
  if food_left = fruit_limit && not state.fruit_generated then 
    begin 
      generate_fruit state.map;
      {state with points = points + value; 
                  food_left = food_left; 
                  fruit_generated = true}
    end 
  else {state with points = points + value; food_left = food_left}

let update_state_lives state map = 
  let player_pos = Player.get_position (player state) in
  let new_lives = ref (lives state) in
  let ghosts = ghosts state in
  for i = 0 to (Array.length ghosts) - 1 do
    let ghost_pos = Ghost.get_position ghosts.(i) in
    if player_pos = ghost_pos then
      new_lives := (!new_lives -1)
  done;
  {state with lives = !new_lives}



(*{
  points = state.points + 1;
  lives = 3;
  ghosts = state.ghosts;
  current_level = 1;
  map = map;
  follower_ghosts = []
  }*)

let new_follower state ghost = 
  {state with follower_ghosts = ghost::(followers state)}

let remove_follower state ghost = 
  let rec find_follower acc = function 
    |[] -> {state with follower_ghosts = acc}
    |h::t -> begin 
        if h = ghost 
        then find_follower acc t 
        else find_follower (h::acc) t
      end 
  in 
  find_follower [] (followers state)

let make_ghosts num min_x min_y = 
  let color_list = [|"cyan"; "pink"; "red"; "orange"|] in 
  let rec set_ghosts_helper acc counter = function 
    | n when n>0 -> begin 
        let x = min_x + (50 * counter) in 
        let y = min_y in 
        let color = color_list.(counter) in 
        let new_g = new_ghost x y (0,50) color in 
        set_ghosts_helper (new_g::acc) (counter + 1) (n - 1)
      end 
    | _ -> acc 
           |> List.rev 
           |> Array.of_list 
  in 
  set_ghosts_helper [] 0 num


let lives_img_lst state = 
  let rec make_lst acc = function 
    | n when n > 0 -> 
      let image = sprite_from_sheet sprite_sheet 8 1 50 50 2 in 
      make_lst (image::acc) (n-1)
    | _ -> acc
  in 
  make_lst [] (lives state)
