open Graphics
open Images
open Map
open Player
open State
open Ghost
open Sprite

let window_width = 1500
let window_height = 750
let map_width = 1300
let map_height = 550

let ghost_radius = 25
let player_radius = 25 

let move_amt = 50 

let game_status state = 
  ("Points: " ^ string_of_int (points state)
   ^ "   Lives: " ^ string_of_int (lives state)
   ^ "   Level: " ^ string_of_int (current_level state))

let tile_type str = ("Tile type: " ^ str)

let food_count food = ("Food Count: " ^ string_of_int (food))

let check_move b = ("Can Move: " ^ string_of_bool(b))

let set_window (title: string) (color: Graphics.color) : unit = 
  set_window_title title;
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ())

let num_ghosts = 2

(** [parse_dir] is the tuple representing the change in coordinates from the 
    user's character input. *)
let parse_dir (dir: char) =
  match dir with 
  |'w' ->  (0, move_amt)
  |'s' -> (0, -move_amt)
  |'a' -> (-move_amt, 0)
  |'d' -> (move_amt, 0)
  |_ -> (0,0)

(** [rand_char] is the char for a movement as determined by [num]. For ghost 
    movement, [num] is a random int in the range [0,3]. *)
let rand_char num = 
  match num with 
  | 0 -> 'w'
  | 1 -> 's'
  | 2 -> 'a'
  | 3 -> 'd'
  | _ -> 'z'

(** [move_ghosts] randomly moves each ghost to a neighboring cell, so long as 
    it is a viable move. *)
let move_ghosts ghosts map = 
  let rec new_g_pos (g : Ghost.t) (dir : int * int) =  
    if Map.check_move (Ghost.get_pos g) map dir 
    then Ghost.move g dir 
    else new_g_pos g (parse_dir (rand_char (Random.self_init (); Random.int 4)))
  in 
  Array.iter (fun g -> 
      new_g_pos g (Random.self_init (); parse_dir 
                     (rand_char (Random.self_init (); Random.int 4)))) 
    ghosts 

let flush () = 
  while Graphics.key_pressed () do 
    ignore (Graphics.read_key());
  done;
  ()

(** [pick_move] is the viable move that the player can make given their current
    command and their previous move. *)
let pick_move user map next prev  = 
  if Map.check_move (get_position user) map next
  then next else
  if Map.check_move (get_position user) map prev
  then prev
  else (0,0)

(** [make_move] moves the player in the direction specificed by [dir] and 
    consumes food on the new tile, if there is any food. *)
let make_move user map dir  = 
  Player.move user dir; 
  check_food (get_position user) map

let rec loop () user map state ghosts prev_move prev_move_attempt = 
  Unix.sleepf(0.3);
  let next_move = 
    if Graphics.key_pressed () 
    then parse_dir (Graphics.read_key ())
    else prev_move_attempt 
  in 
  flush ();
  let current_move = pick_move user map next_move prev_move in 
  make_move user map current_move;
  draw_current_map map;
  (*draw_image map_image 0 0;*)
  let new_state = State.update_state_food state map in
  draw_state new_state;
  (*draw_string (tile_type (Map.get_tile_type (get_position user) map));*)
  (*draw_string (check_move (Map.check_move (get_position user) map dir));*)
  draw_player user;
  move_ghosts ghosts map; 
  draw_ghosts (ghosts);
  loop () user map state ghosts current_move next_move

and draw_ghosts ghosts = 
  set_color cyan;
  for i = 0 to Array.length ghosts - 1 do 
    let g = ghosts.(i) in 
    fill_circle (fst (get_pos g)) (snd (get_pos g)) ghost_radius;
  done

and draw_player user = 
  let x = fst (get_position user) in 
  let y = snd (get_position user) in 
  set_color yellow; 
  fill_circle x y player_radius
(* let x = fst (get_position user) in 
   let y = snd (get_position user) in 
   draw_image ((sprite_image (player_image new_player))) 
   (x-player_radius) (y-player_radius); *)
(* Graphic_image.draw_image (sprite_image (player_image new_player)) 
   150 150; *)
and draw_current_map map = 
  clear_graph ();
  set_window "Pacman" black;
  set_color blue;
  draw_map map

and draw_state state = 
  moveto 175 75;
  set_color red;
  draw_string (game_status state)

let main (settings: string) : unit = 
  open_graph settings;
  set_window "Pacman" black;
  set_color black;
  fill_rect 0 0 window_width window_height;
  (* set_color blue;
     draw_rect 100 100 map_width map_height; *)
  let map = make_map (100,100) "standard" in 
  draw_map map;
  (* let map_image = get_image 0 0 window_width window_height in  *)
  set_color yellow; 
  (* draw_image ((sprite_image (player_image new_player))) 
     150 150; *)
  (* Graphic_image.draw_image (sprite_image (player_image new_player)) 
     150 150; *)
  fill_circle 175 175 player_radius;
  moveto 175 75;
  let state = initial_state map (State.make_ghosts num_ghosts 675 375) in 
  set_color cyan; 
  Array.iter (fun g -> 
      fill_circle (fst (get_pos g)) (snd (get_pos g)) ghost_radius) 
    (ghosts state);
  set_color red;
  set_text_size 32;
  draw_string (game_status state);
  ignore (loop () new_player map state (ghosts state) (0,0) (0,0));
  ()

let () = 
  let settings = 
    string_of_int window_width ^ "x" ^ string_of_int window_height
  in
  main (" " ^ settings) 

(* 
let parse_dir (user: Player.t) (dir: string) =
  match dir with 
  |"\033[A" -> (0,50)
  |"\033[B" -> (0,-50)
  |"\033[D" -> (-50,0)
  |"\033[C" -> (50,0)
  |_ -> (0,0)  *)