open Ghost
open Map
open Player
open Sprite

type t = {
  player : Player.t;
  points : int;
  lives : int;
  ghosts : Ghost.t array;
  current_level : int;
  map : Map.t;
  follower_ghosts : Ghost.t list
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
  points = 5;
  lives = 3;
  ghosts = ghosts_entry;
  current_level = 1;
  map = map;
  follower_ghosts = []
} 

let update_state_food state map = 
  {state with points = (points state) + 1}

(* {
   points = state.points + 1;
   lives = 3;
   ghosts = state.ghosts;
   current_level = 1;
   map = map;
   follower_ghosts = []
   } *)

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

let make_ghost_sprites (color : string) : Sprite.t list= 
  let dir = [|"_up.png"; "_right.png"; "_down.png"; "_left.png"|] in 
  (* for i = 0 to 3 do 
     let sprite = (make_sprite (color ^ dir.(i))) in 
     (sprite::acc)
     done; *)

  let rec helper_initialize_sprites acc counter = function 
    | n when n>=0 -> let sprite = (make_sprite (color ^ dir.(counter))) in     
      helper_initialize_sprites (sprite::acc) (counter + 1) (n - 1)
    | _ -> acc
  in 
  helper_initialize_sprites [] 0 3 


let make_ghosts num min_x min_y = 
  let color_list = [|"cyan"|] in 
  let rec set_ghosts_helper acc counter = function 
    | n when n>0 -> begin 
        let x = min_x + (50 * counter) in 
        let y = min_y in 
        let sprites = (make_ghost_sprites color_list.(counter)) in 
        let new_g = new_ghost x y (0,50) sprites in 
        set_ghosts_helper (new_g::acc) (counter + 1) (n - 1)
      end 
    | _ -> acc 
           |> List.rev 
           |> Array.of_list 
  in 
  set_ghosts_helper [] 0 num


let lives_img_lst state = 
  let rec make_lst acc = function 
    | n when n>0 -> make_lst ((make_sprite "cherry.png")::acc) (n-1)
    | _ -> acc
  in 
  make_lst [] (lives state)
