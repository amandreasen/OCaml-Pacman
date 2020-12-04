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

let move_amt = 15

let sleep_time = 0.05

let tile_type str = ("Tile type: " ^ str)

let food_count food = ("Food Count: " ^ string_of_int (food))

let check_move b = ("Can Move: " ^ string_of_bool(b))

let set_window (title: string) (color: Graphics.color) : unit = 
  set_window_title title;
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ())

let num_ghosts = 1

(** [closeness_threshold] encompassess the nearest 8 tiles around the user.  *)
let closeness_threshold = 50.0 *. sqrt 2.0  

(** [max_follow_time] is the maximum number of seconds that a ghost will follow
    the user. *)
let max_follow_time = 5.0 (*100. *. sleep_time*)

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

(** [number_sign] is 1 if n is nonnegative and -1 if n is negative. *)
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

(** [position_diff] is the user's position minus the ghost's position. *)
let position_diff ghost user = 
  let x_g = fst (Ghost.get_position ghost) in 
  let x_u = fst (Player.get_position user) in 
  let y_g = snd (Ghost.get_position ghost) in 
  let y_u = snd (Player.get_position user) in 
  (x_u - x_g, y_u - y_g)

(** [randomly_move_ghost] tries random moves until the ghost makes a successful 
    move. *)
let rec randomly_move_ghost ghost map dir = 
  if Map.check_move (get_position ghost) map dir 
  then Ghost.move ghost dir 
  else randomly_move_ghost ghost map 
      (parse_dir (rand_char (Random.self_init (); Random.int 4)))

(** [will_follow] is true if the ghost successfully makes a move that follows 
    the player. If the ghost is not successful in following the player then the 
    ghost makes a random move. *)
let will_follow ghost map dir_attempt try_random_move random_move = 
  let pos = get_position ghost in 
  let continue =  Map.check_move pos map dir_attempt in 
  if continue 
  then begin start_following ghost; 
    Ghost.move ghost dir_attempt; 
    continue end 
  else 
  if try_random_move 
  then begin randomly_move_ghost ghost map random_move; 
    continue end 
  else continue

(** [helper_following_make_move] finds the direction the ghost needs to move in
    to follow the player and then tries to make that move. If the move is not 
    possible then the ghost makes a random move. *)
let helper_following_make_move ghost map position_difference x_sign y_sign = 
  let rand_dir = Random.self_init (); 
    Random.int 4 |> rand_char |> parse_dir in 
  match position_difference with
  | (n,0) -> ignore (will_follow ghost map (x_sign * move_amt, 0) true rand_dir)
  | (0,m) -> ignore (will_follow ghost map (0, y_sign * move_amt) true rand_dir)
  | (n,m) -> begin 
      if not (will_follow ghost map (x_sign * move_amt, 0) false rand_dir)
      then 
        if not (will_follow ghost map (0, y_sign * move_amt) false rand_dir)
        then randomly_move_ghost ghost map rand_dir 
    end 

(** [move_ghost_following] calculates the difference in positions between the 
    ghost and player and then uses the tuple as an input to 
    [helper_following_make_move].*)
let move_ghost_following ghost map user = 
  incr_following_count ghost;
  let position_difference = position_diff ghost user in 
  let x_sign = position_difference |> fst |> number_sign in 
  let y_sign = position_difference |> snd |> number_sign in 
  helper_following_make_move ghost map position_difference x_sign y_sign

(** [try_ghost_follow] determines whether the ghost should start following the 
    player or move randomly, and then makes that move. *)
let try_ghost_follow g map user= 
  reset_following g; 
  if are_close g user 
  then move_ghost_following g map user 
  else 
    begin 
      if Map.check_move (get_position g) map (prev_move g)
      then Ghost.move g (prev_move g) 
      else randomly_move_ghost g map
          (Random.self_init (); 
           Random.int 4 |> rand_char |> parse_dir) 
    end 

(** [move_ghosts] moves each ghost to either continue following the player, 
    stop following the player if they have been following for too long, if the
    ghost is now within the [closeness_threshold] then it starts to follow the 
    player, or it simply continues it's previous move until it is no longer 
    possible - at this point the ghost will make a random move. *)
let move_ghosts ghosts map (user : Player.t) = 
  Array.iter (fun g ->
      if is_following g && following_counter g <= int_of_float max_follow_time 
      then move_ghost_following g map user 
      else try_ghost_follow g map user)
    ghosts 

(** [flush] clears the user's inputs. *)
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
let rec loop state map_image  = 
  let user = player state in 
  let map = map state in 
  let ghosts = ghosts state in
  move_player user map; 
  let new_state = State.update_state_food state map in
  (*draw_string (tile_type (Map.get_tile_type (get_position user) map));*)
  (*draw_string (check_move (Map.check_move (get_position user) map dir));*)
  move_ghosts ghosts map user; 
  draw_game new_state map_image;
  synchronize ();
  Unix.sleepf(sleep_time); 
  loop state map_image 

and move_player user map  = 
  let prev_move = player_prev_move user in 
  let prev_move_attempt = player_prev_attempt user in 
  let next_move = 
    if Graphics.key_pressed () 
    then parse_dir (Graphics.read_key ())
    else prev_move_attempt 
  in 
  flush ();
  let current_move = pick_move user map next_move prev_move in 
  make_move user map current_move;
  move_attempt user next_move;

and draw_game state map_image = 
  draw_current_map (map state) map_image;
  draw_state state;
  draw_player (player state);
  draw_ghosts (ghosts state);

  (** TODO: use sprite display instead of circle *)
and draw_ghosts ghosts = 
  set_color cyan;
  for i = 0 to Array.length ghosts - 1 do 
    let g = ghosts.(i) in 
    let pos = Ghost.get_position g in 
    let x = fst pos in 
    let y = snd pos in 
    fill_circle x y ghost_radius;
  done

(** TODO: update direction when needed - also import more sprites *)
and draw_player user = 
  let x = fst (Player.get_position user) in 
  let y = snd (Player.get_position user) in 
  let image = user 
              |> player_image 
              |> sprite_image 
              |> Graphic_image.of_image 
  in 
  Graphics.draw_image image (x-player_radius) (y-player_radius)

and draw_current_map (map: Map.t) (map_image: Graphics.image) = 
  Graphics.draw_image map_image 0 0;
  Map.draw_food map;
  set_color blue;
  draw_map map

and draw_state state = 
  moveto 175 75;
  set_color red;
  draw_string ("Points: " ^ string_of_int (points state));
  moveto 175 675;
  draw_string ("Level: " ^ string_of_int (current_level state));
  draw_lives state;

and draw_lives state = 
  let rec draw_helper prev_pos = function 
    | [] -> ()
    | h::t -> begin 
        let x = (fst prev_pos) + 50 in 
        let y = snd prev_pos in 
        (** TODO: make the cherry png 50x50 for use here *)
        (* let img = h |> sprite_image |> Graphic_image.of_image in 
           Graphics.draw_image img x y; *)
        set_color red;
        draw_circle x y 24;
        draw_helper (x,y) t
      end
  in 
  draw_helper (1125,75) (lives_img_lst state) 

let window_init (settings: string) : unit = 
  open_graph settings;
  set_window "Pacman" black;
  set_color black;
  fill_rect 0 0 window_width window_height

let map_init (map: Map.t): Graphics.image = 
  draw_map map;
  Graphics.get_image 0 0 window_width window_height 

let ghost_helper (ghost: Ghost.t) : unit = 
  let pos = get_position ghost in 
  let x = fst pos in 
  let y = snd pos in 
  fill_circle x y ghost_radius

let main (settings: string) : unit = 
  window_init settings;
  let map = make_map (100,100) "OCaml" in 
  let map_background = map_init map in
  set_color yellow; 
  (* fill_circle 175 175 player_radius;
     moveto 175 75; *)
  let player = new_player in 
  let ghosts = State.make_ghosts num_ghosts 725 375 in 
  let state = initial_state player map ghosts in 
  set_color cyan; 
  Array.iter ghost_helper ghosts; (** replace with draw ghosts (?) *)
  set_color red;
  set_text_size 32;
  (* draw_string (game_status state); *)
  auto_synchronize false;
  ignore (loop state map_background);
  ()

let () = 
  let settings = 
    string_of_int window_width ^ "x" ^ string_of_int window_height
  in
  main (" " ^ settings) 