open Graphics
(* game constants *)
let window_width = 1500
let window_height = 750
let map_width = 1300
let map_height = 550

let ghost_radius = 25
let player_radius = 25 

let player_width = 50
let player_height = 50
let init_pos = (175, 175)

let move_amt = 10

let sleep_time = 0.035

(** [closeness_threshold] encompassess the nearest 8 tiles around the user.  *)
let closeness_threshold = 50.0 *. sqrt 2.0  

(** [max_follow_time] is the maximum number of seconds that a ghost will follow
    the user. *)
let max_follow_time = 100. *. sleep_time

let max_role_rev_time = 75. *. sleep_time

let sprite_sheet = 
  let sheet = Png.load_as_rgb24 ("./sprites/sprite_sheet.png") [] in 
  let black_box = Png.load_as_rgb24 ("./sprites/black.png") [] in 
  Images.blit black_box 0 0 sheet 100 45 305 100;
  sheet

(* map constants *)

(* point value for food *)
let food_val = 1

(* point value for special food *)
let special_val = 5 

(* Constants *)
let tile_size = 50
let wall_width = 20

let food_radius = 3
let special_radius = 8

let fruit_width = 50
let fruit_height = 50


let food_color = rgb 255 184 245
let special_color = rgb 255 171 196
let wall_color = Graphics.blue
let check_tile_size = 51

(*State constants*)
let fruit_limit = 50
let fruit_timer = 300

let png_wl = 50