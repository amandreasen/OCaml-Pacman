(* game constants *)
let window_width = 1500
let window_height = 750
let map_width = 1300
let map_height = 550

let ghost_radius = 25
let player_radius = 25 

let move_amt = 15

let sleep_time = 0.05

let num_ghosts = 1

(** [closeness_threshold] encompassess the nearest 8 tiles around the user.  *)
let closeness_threshold = 50.0 *. sqrt 2.0  

(** [max_follow_time] is the maximum number of seconds that a ghost will follow
    the user. *)
let max_follow_time = 5.0 (*100. *. sleep_time*)

let sprite_sheet = 
  let sheet = Png.load_as_rgb24 ("./sprites/sprite_sheet.png") [] in 
  let black_box = Png.load_as_rgb24 ("./sprites/black.png") [] in 
  Images.blit black_box 0 0 sheet 100 45 305 100;
  sheet

let food_val = 1
let special_val = 5 