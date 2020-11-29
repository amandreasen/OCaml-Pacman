open Graphics
open Graphic_image
open Images
open Map
open Player
open State
open Ghost
open Sprite

(* game constants *)
let window_width = 1500
let window_height = 750
let map_width = 1300
let map_height = 550

let ghost_radius = 25
let player_radius = 25 

let move_amt = 50 

let sleep_time = 0.3

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

(** circle of radius [closeness_threshold] encompassess the nearest 8 tiles 
    around the user.  *)
let closeness_threshold = 50.0 *. sqrt 2.0  

(** [max_follow_time] is the maximum number of seconds that a ghost will follow
    the user. *)
let max_follow_time = 5.0

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
  | _ -> ' '

let number_sign n = 
  match n with 
  | n when n >= 0 -> 1
  | _ -> -1

(** [are_close] is true if the distance between the [ghost] and [user] are 
    less than or equal to the [closeness_threshold]. *)
let are_close ghost user  = 
  let x_g = fst (Ghost.get_position ghost) in 
  let x_u = fst (Player.get_position user) in 
  let y_g = snd (Ghost.get_position ghost) in 
  let y_u = snd (Player.get_position user) in 
  let x_squared = (x_g + x_u) * (x_g + x_u) in 
  let y_squared = (y_g + y_u) * (y_g + y_u) in 
  let distance = sqrt (float_of_int (x_squared - y_squared)) in 
  distance <= closeness_threshold

let position_diff ghost user = 
  let x_g = fst (Ghost.get_position ghost) in 
  let x_u = fst (Player.get_position user) in 
  let y_g = snd (Ghost.get_position ghost) in 
  let y_u = snd (Player.get_position user) in 
  (x_u - x_g, y_u - y_g)

let will_follow ghost map dir_attempt = 
  let continue =  Map.check_move (get_position ghost) map dir_attempt in 
  if continue then Ghost.move ghost dir_attempt;
  continue

let start_follow ghost dir = 
  start_following ghost
(* set_prev_move ghost dir  *)

let rec randomly_move_ghost ghost map dir = 
  if Map.check_move (get_position ghost) map dir 
  then (* set_prev_move ghost dir;  *)
    Ghost.move ghost dir 
  else randomly_move_ghost ghost map 
      (parse_dir (rand_char (Random.self_init (); Random.int 4)))

(** TODO: there's a lot of repeated code here. rochelle will get to that soon *)
let move_ghost_following ghost map user = 
  incr_following_count ghost;
  let position_difference = position_diff ghost user in 
  let x_sign = position_difference |> fst |> number_sign in 
  let y_sign = position_difference |> snd |> number_sign in 
  match position_difference with
  | (n,0) -> let dir_attempt = (x_sign * move_amt, 0) in 
    if will_follow ghost map dir_attempt 
    then start_follow ghost dir_attempt
  | (0,m) -> let dir_attempt = (0, y_sign * move_amt) in 
    if will_follow ghost map dir_attempt 
    then start_follow ghost dir_attempt
  | (n,m) -> let dir_attempt = (x_sign * move_amt, 0) in 
    if will_follow ghost map dir_attempt 
    then start_follow ghost dir_attempt
    else let dir_attempt = (0, y_sign * move_amt) in
      if will_follow ghost map dir_attempt 
      then start_follow ghost dir_attempt
      else randomly_move_ghost ghost map 
          (Random.self_init (); 
           parse_dir (rand_char (Random.self_init (); Random.int 4)))

let move_ghosts ghosts map (user : Player.t) = 
  Array.iter (fun g ->
      if is_following g && following_counter g <= int_of_float max_follow_time 
      then move_ghost_following g map user 
      else 
        begin 
          reset_following g; 
          if are_close g user 
          then move_ghost_following g map user 
          else 
            begin 
              if Map.check_move (get_position g) map (prev_move g)
              then Ghost.move g (prev_move g) 
              else randomly_move_ghost g map
                  (Random.self_init (); 
                   parse_dir (rand_char (Random.self_init (); Random.int 4))) 
            end 
        end) 
    ghosts 

let flush () = 
  while Graphics.key_pressed () do 
    ignore (Graphics.read_key());
  done;
  ()

(** [pick_move] is the viable move that the player can make given their current
    command and their previous move. *)
let pick_move (user : Player.t) map next prev  = 
  let user_pos = Player.get_position user in 
  if Map.check_move user_pos map next
  then next else
  if Map.check_move user_pos map prev
  then prev
  else (0,0)

(** [make_move] moves the player in the direction specificed by [dir] and 
    consumes food on the new tile, if there is any food. *)
let make_move user map dir  = 
  Player.move user dir; 
  check_food (Player.get_position user) map

(** [prev_move] is the actual move that the user just made. 
    [prev_move_attempt] is their last input that may or may not have passed. *)
let rec loop state map_image prev_move prev_move_attempt = 
  let user = player state in 
  let map = map state in 
  let ghosts = ghosts state in
  Unix.sleepf(sleep_time);
  let next_move = 
    if Graphics.key_pressed () 
    then parse_dir (Graphics.read_key ())
    else prev_move_attempt 
  in 
  flush ();
  let current_move = pick_move user map next_move prev_move in 
  make_move user map current_move;
  draw_current_map map map_image;
  (*draw_image map_image 0 0;*)
  let new_state = State.update_state_food state map in
  draw_state new_state;
  (*draw_string (tile_type (Map.get_tile_type (get_position user) map));*)
  (*draw_string (check_move (Map.check_move (get_position user) map dir));*)
  draw_player user;
  move_ghosts ghosts map user; 
  draw_ghosts (ghosts);
  loop state map_image current_move next_move

and draw_ghosts ghosts = 
  set_color cyan;
  for i = 0 to Array.length ghosts - 1 do 
    let g = ghosts.(i) in 
    let pos = Ghost.get_position g in 
    let x = fst pos in 
    let y = snd pos in 
    fill_circle x y ghost_radius;
  done

and draw_player user = 
<<<<<<< HEAD
  (* let x = fst (get_position user) in 
     let y = snd (get_position user) in 
     set_color yellow; 
     fill_circle x y player_radius *)
  let x = fst (Player.get_position user) in 
  let y = snd (Player.get_position user) in 
  let sprite = (sprite_image (player_image new_player)) in
  let image = Graphic_image.of_image sprite in 
  Graphics.draw_image image (x-player_radius) (y-player_radius)

(* draw_image ((sprite_image (player_image new_player))) 
=======
  let pos = Player.get_position user in
  let x = fst pos in 
  let y = snd pos in 
  set_color yellow; 
  fill_circle x y player_radius
(* let x = fst (Player.get_position user) in 
   let y = snd (Player.get_position user) in 
   Graphic_image.draw_image ((sprite_image (player_image new_player))) 
>>>>>>> b95c640f7e502552594f69aee2eff5ecd3437203
   (x-player_radius) (y-player_radius); *)
(* Graphic_image.draw_image (sprite_image (player_image new_player)) 
   150 150; *)
and draw_current_map (map: Map.t) (map_image: Graphics.image) = 
  clear_graph ();
  Graphics.draw_image map_image 0 0;
  Map.draw_food map;
  set_color blue;
  draw_map map

and draw_state state = 
  moveto 175 75;
  set_color red;
  draw_string (game_status state)

let window_init (settings: string) : unit = 
  open_graph settings;
  set_window "Pacman" black;
  set_color black;
  fill_rect 0 0 window_width window_height

let map_init (map: Map.t): Graphics.image = 
  draw_map map;
  get_image 0 0 window_width window_height 

let ghost_helper (ghost: Ghost.t) : unit = 
  let pos = get_position ghost in 
  let x = fst pos in 
  let y = snd pos in 
  fill_circle x y ghost_radius

let main (settings: string) : unit = 
  window_init settings;
  let map = make_map (100,100) "OCaml" in 
  let map_background = map_init map in
  (* let map_image = get_image 0 0 window_width window_height in  *)
  set_color yellow; 
  (* draw_image ((sprite_image (player_image new_player))) 
     150 150; *)
  (* Graphic_image.draw_image (sprite_image (player_image new_player)) 
     150 150; *)
  fill_circle 175 175 player_radius;
  moveto 175 75;
  let player = new_player in 
  let ghosts = State.make_ghosts num_ghosts 725 375 in 
  let state = initial_state player map ghosts in 
  set_color cyan; 
  Array.iter ghost_helper ghosts;
  set_color red;
  set_text_size 32;
  draw_string (game_status state);
  ignore (loop state map_background (0,0) (0,0));
  ()

let () = 
  let settings = 
    string_of_int window_width ^ "x" ^ string_of_int window_height
  in
  main (" " ^ settings) 