open Ghost
open Map
open Player
open Sprite
open Constants
open Graphics 
open Graphic_image

(* constants *)
let fruit_limit = 50
let fruit_timer = 200

type t = {
  player : Player.t;
  points : int;
  lives : int;
  ghosts : Ghost.t array;
  map : Map.t;
  map_background : Graphics.image;
  follower_ghosts : Ghost.t list;
  food_left: int;
  fruit_generated: bool;
  fruit_active: bool;
  mutable fruit_timer: int;
}

let player state = 
  state.player

let points state =
  state.points

let lives state =
  state.lives

let ghosts state =
  state.ghosts

let followers state = 
  state.follower_ghosts

let map state = 
  state.map

let initial_state player map ghosts_entry map_background = {
  player = player;
  points = 0;
  lives = 3;
  ghosts = ghosts_entry;
  map = map;
  map_background = map_background;
  follower_ghosts = [];
  food_left = food_count map;
  fruit_generated = false;
  fruit_active = false;
  fruit_timer = 0;
} 

let update_state_food (state: t) (value: int) = 
  let points = state.points in
  let food_left = 
    if value = food_val || value = special_val then state.food_left - 1 
    else state.food_left
  in
  if food_left = fruit_limit && not state.fruit_generated then 
    begin 
      generate_fruit state.map;
      {state with points = points + value; 
                  food_left = food_left; 
                  fruit_generated = true;
                  fruit_active = true;
                  fruit_timer = fruit_timer}
    end 
  else {state with points = points + value; food_left = food_left}

let update_state_lives state = 
  let player_pos = Player.get_position (player state) in
  let new_lives = ref (lives state) in
  let ghosts = ghosts state in
  for i = 0 to (Array.length ghosts) - 1 do
    let ghost_pos = Ghost.get_position ghosts.(i) in
    if player_pos = ghost_pos then
      new_lives := (!new_lives -1)
  done;
  {state with lives = !new_lives}

let update_state (state: t) : t = 
  let state' = update_state_lives state in 
  let player_pos = Player.get_position state.player in 
  let point_val = Map.get_tile_value player_pos state.map in
  let state'' = update_state_food state' point_val in 
  let timer = 
    if state''.fruit_timer > 0 then state''.fruit_timer - 1 else 0 
  in 
  if timer = 0 && state''.fruit_active then 
    begin 
      clear_fruit state.map;
      {state'' with fruit_timer = timer; fruit_active = false}
    end 
  else {state'' with fruit_timer = timer}

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
let rec randomly_move_ghost (ghost: Ghost.t) map dir = 
  if Map.check_move (Ghost.get_position ghost) map dir 
  then Ghost.move ghost dir 
  else randomly_move_ghost ghost map 
      (parse_dir (rand_char (Random.self_init (); Random.int 4)))

(** [will_follow] is true if the ghost successfully makes a move that follows 
    the player. If the ghost is not successful in following the player then the 
    ghost makes a random move. *)
let will_follow ghost map dir_attempt try_random_move random_move = 
  let pos = Ghost.get_position ghost in 
  let continue =  Map.check_move pos map dir_attempt in 
  if continue 
  then 
    begin 
      start_following ghost; 
      Ghost.move ghost dir_attempt; 
      continue 
    end 
  else 
  if try_random_move 
  then 
    begin 
      randomly_move_ghost ghost map random_move; 
      continue
    end 
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
  if following_counter g > int_of_float max_follow_time
  then 
    begin 
      reset_following g; randomly_move_ghost g map
        (Random.self_init (); 
         Random.int 4 |> rand_char |> parse_dir) 
    end 
  else 
  if are_close g user 
  then move_ghost_following g map user 
  else 
    begin 
      if Map.check_move (Ghost.get_position g) map (prev_move g)
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
      if (is_following g || are_close g user) 
      && following_counter g <= int_of_float max_follow_time 
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
  then next 
  else
    begin 
      if Map.check_move user_pos map prev
      then prev
      else (0,0)
    end 

(** [make_move] moves the player in the direction specificed by [dir] and 
    consumes food on the new tile, if there is any food. *)
let make_move user map dir  = 
  Player.move user dir

let draw_ghosts ghosts = 
  set_color cyan;
  for i = 0 to Array.length ghosts - 1 do 
    let g = ghosts.(i) in 
    let pos = Ghost.get_position g in 
    let x = fst pos in 
    let y = snd pos in 
    let image = g
                |> get_sprite 
                |> sprite_image 
                |> Graphic_image.of_image 
    in 
    Graphics.draw_image image (x - ghost_radius) (y - ghost_radius)
  done

(** TODO: update direction when needed - also import more sprites *)
let draw_player user = 
  let pos = Player.get_position user in 
  let x = fst pos in 
  let y = snd pos in 
  let image = user 
              |> player_image 
              |> sprite_image 
              |> Graphic_image.of_image 
  in 
  Graphics.draw_image image (x - player_radius) (y - player_radius)

let draw_current_map (map: Map.t) (map_image: Graphics.image) = 
  Graphics.draw_image map_image 0 0;
  Map.draw_food map;
  set_color blue;
  draw_map map

let draw_lives state = 
  let rec draw_helper prev_pos = function 
    | [] -> ()
    | h::t -> begin 
        let x = (fst prev_pos) + 50 in 
        let y = snd prev_pos in 
        let img = h |> sprite_image |> Graphic_image.of_image in 
        Graphics.draw_image img x y;
        draw_helper (x,y) t
      end
  in 
  draw_helper (1125,60) (lives_img_lst state) 

let draw_state state user = 
  (* draw_string (Map.get_tile_type (Player.get_position user) (map state)); *)
  draw_lives state

let move_player user map key = 
  let prev_move = player_prev_move user in 
  let next_move = parse_dir key in 
  let current_move = pick_move user map next_move prev_move in 
  make_move user map current_move;
  move_attempt user next_move

let draw_game state = 
  draw_current_map state.map state.map_background;
  draw_state state state.player;
  draw_player state.player;
  draw_ghosts state.ghosts

let map_init (map: Map.t): Graphics.image = 
  draw_map map;
  Graphics.get_image 0 0 window_width window_height 

let init_level (map_name: string): t =
  let map = make_map (100, 100) map_name in 
  let map_background = map_init map in
  let player = new_player in 
  let ghosts = 
    match map_name with 
    | "OCaml" -> make_ghosts num_ghosts 725 375 
    | "standard" -> make_ghosts num_ghosts 725 375
    | _ -> failwith "Invalid map!"
  in
  initial_state player map ghosts map_background

let update_level (state: t) (key: char) : t = 
  let user = state.player in 
  let map = state.map in 
  let ghosts = state.ghosts in
  move_player user map key;  
  move_ghosts ghosts map user; 
  let state' = update_state state in
  check_food (Player.get_position user) map;
  draw_game state';
  state'

let check_win (state: t) : int = 
  if state.food_left = 0 then 1 
  else if state.lives = 0 then -1 
  else 0
