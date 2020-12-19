open Ghost
open Map
open Player
open Sprite
open Constants
open Graphics 
open Graphic_image

(* constants *)


type game_state = Active | Waiting | Dying | Ended

type t = {
  map_name: string;
  player : Player.t;
  points : int;
  lives : int;
  ghosts : Ghost.t array;
  num_ghosts : int;
  map : Map.t;
  map_background : Graphics.image;
  follower_ghosts : Ghost.t list;
  food_left: int;
  fruit_generated: bool;
  fruit_active: bool;
  fruit_eaten: bool;
  mutable fruit_timer: int;
  game_state: game_state;
  mutable role_reversed: bool;
  mutable reversal_timer : int
}

let points state =
  state.points

let lives state =
  state.lives

let fruit_eaten state =   
  state.fruit_eaten

let reset_player state = 
  {state with player = new_player ()}

let initial_state player map ghosts_entry map_background map_name = {
  map_name = map_name;
  player = player;
  points = 0;
  lives = 15;
  ghosts = ghosts_entry;
  num_ghosts = Array.length ghosts_entry;
  map = map;
  map_background = map_background;
  follower_ghosts = [];
  food_left = food_count map;
  fruit_generated = false;
  fruit_active = false;
  fruit_eaten = false;
  fruit_timer = 0;
  game_state = Active;
  role_reversed = false;
  reversal_timer = 0;
} 

let update_state_food (value: int) (state: t) = 
  let points = state.points in
  let food_left = 
    if value = food_val || value = special_val then state.food_left - 1 
    else state.food_left
  in
  let fruit_eaten = if value > 10 then true else state.fruit_eaten in
  let state' = {state with points = points + value; 
                           food_left = food_left; 
                           fruit_eaten = fruit_eaten;} 
  in
  if food_left = fruit_limit && not state.fruit_generated then 
    begin 
      generate_fruit state.map;
      {state' with fruit_generated = true;
                   fruit_active = true;
                   fruit_timer = fruit_timer;}
    end 
  else state'

let check_overlap user_pos acc ghost =
  let ghost_pos = Ghost.get_position ghost in
  let ghost_x = fst ghost_pos in 
  let ghost_y = snd ghost_pos in 
  let user_x = fst user_pos in 
  let user_y = snd user_pos in 
  let threshold = png_wl / 2 in
  let overlap = 
    (Int.abs (ghost_x - user_x) <= threshold && user_y = ghost_y) || 
    (Int.abs (ghost_y - user_y) <= threshold && user_x = ghost_x)
  in
  acc || overlap

let update_special_food state pts = 
  if pts = special_val 
  then 
    {state with role_reversed = true; reversal_timer = 1} 
  else begin 
    (* moveto 500 75; 
       print_string "not special  "; *)
    if state.reversal_timer >= int_of_float max_role_rev_time 
    then {state with role_reversed = false; reversal_timer = 0}
    else state
  end 


let update_game_state state map = 
  let player_pos = Player.get_position state.player in
  let ghosts = state.ghosts in
  let overlap = Array.fold_left (check_overlap player_pos) false ghosts in 
  if overlap then {state with game_state = Waiting; ghosts = [||]} else state

let update_state (state: t) : t = 
  let player_pos = Player.get_position state.player in 
  let point_val = Map.get_tile_value player_pos state.map in
  let state' = update_game_state state state.map 
               |> update_state_food point_val 
  in 
  let timer = 
    if state'.fruit_timer > 0 then state'.fruit_timer - 1 else 0 
  in 
  let state'' = update_special_food state' point_val in 
  if timer = 0 && state'.fruit_active then 
    begin 
      clear_fruit state.map;
      {state'' with fruit_timer = timer; fruit_active = false}
    end 
  else {state'' with fruit_timer = timer} 


let new_follower state ghost = 
  {state with follower_ghosts = ghost :: state.follower_ghosts}

let remove_follower state ghost = 
  let rec find_follower acc = function 
    |[] -> {state with follower_ghosts = acc}
    |h::t -> begin 
        if h = ghost 
        then find_follower acc t 
        else find_follower (h::acc) t
      end 
  in 
  find_follower [] state.follower_ghosts

let make_ghosts_helper num map = 
  let color_list = [|"cyan"; "pink"; "red"; "orange"|] in 
  let positions = ghost_init_positions map in 
  let directions = initial_ghost_moves map in 
  let acc = ref [] in 
  for i = 0 to num - 1 do 
    let color = color_list.(i) in 
    let x = fst positions.(i) in 
    let y = snd positions.(i) in 
    let move = directions.(i).(0) in 
    let new_g = new_ghost x y move color in 
    acc := new_g :: !acc
  done ;
  let ghost_arr = Array.of_list !acc in 
  ghost_arr

let lives_img_lst state = 
  let rec make_lst acc = function 
    | n when n > 0 -> 
      let image = sprite_from_sheet sprite_sheet 8 1 50 50 2 in 
      make_lst (image :: acc) (n - 1)
    | _ -> acc
  in 
  make_lst [] state.lives

(** [parse_dir] is the tuple representing the change in coordinates from the 
    user's character input. *)
let parse_dir (dir: char) =
  match dir with 
  |'w' ->  (0, move_amt)
  |'s' -> (0, -move_amt)
  |'a' -> (-move_amt, 0)
  |'d' -> (move_amt, 0)
  | _ -> (0,0)

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
let position_diff ghost user rev = 
  let x_g = fst (Ghost.get_position ghost) in 
  let x_u = fst (Player.get_position user) in 
  let y_g = snd (Ghost.get_position ghost) in 
  let y_u = snd (Player.get_position user) in 
  if rev then (x_g - x_u, y_g - y_u)
  else (x_u - x_g, y_u - y_g)

let move_ghost_randomly ghost map = 
  let start = Random.self_init (); Random.int 4 in 
  let next = ref start in 
  let dir = ref (!next |> rand_char |> parse_dir) in 
  while not (Map.check_move (Ghost.get_position ghost) map !dir true) do 
    next := (!next + 1) mod 4;
    dir := !next |> rand_char |> parse_dir;
  done;
  Ghost.move ghost !dir 

(* let rec move_ghost_randomly ghost map = 
   let dir = Random.self_init (); Random.int 4 |> rand_char |> parse_dir in 
   if Map.check_move (Ghost.get_position ghost) map dir 
   then Ghost.move ghost dir 
   else move_ghost_randomly ghost map   *)

let move_selector (map: Map.t) (ghost_pos: int * int) (dir: int * int) 
  : int * int = 
  let select = Random.self_init(); Random.int 2 in 
  let dir1 = 
    match dir with 
    | (x, 0) when x <> 0 -> 
      if select = 0 then (0, x) else (0, -x)
    | (0, y) when y <> 0 -> 
      if select = 0 then (y, 0) else (-y, 0)
    | _ -> dir 
  in 
  let dir2 = 
    match dir1 with 
    | (x, 0) -> (-x, 0)
    | (0, y) -> (0, -y)
    | _ -> dir 
  in
  if Map.check_move ghost_pos map dir1 true then dir1
  else if Map.check_move ghost_pos map dir2 true then dir2 
  else dir

let select_move_new (map: Map.t) (ghost_pos: int * int) (dir: int * int) 
  : int * int = 
  let select = Random.self_init(); Random.int 2 in 
  if select = 0 then dir 
  else move_selector map ghost_pos dir

let helper_new_move map ghost prev ghost_pos = 
  let new_move = select_move_new map ghost_pos prev in 
  if Map.check_move (Ghost.get_position ghost) map new_move true
  then Ghost.move ghost new_move
  else move_ghost_randomly ghost map 

let move_ghost_prev ghost map = 
  let dir = Ghost.prev_move ghost in 
  let ghost_pos = Ghost.get_position ghost in 
  if Map.check_move (Ghost.get_position ghost) map dir true
  then Ghost.move ghost dir 
  else helper_new_move map ghost dir ghost_pos

(* if is_done_initializing ghost
   then helper_new_move map ghost dir ghost_pos
   else 
   begin 
    if Map.check_move (Ghost.get_position ghost) map dir false
    then Ghost.move ghost dir 
    else begin finish_initializing ghost; 
      helper_new_move map ghost dir ghost_pos
    end 
   end  *)

let helper_possible_moves ghost user rev = 
  let pos_dif = position_diff ghost user rev in 
  let x_sign = pos_dif |> fst |> number_sign in 
  let y_sign = pos_dif |> snd |> number_sign in 
  let helper_lst_maker = function 
    | (n,0) ->  [(x_sign * move_amt, 0); (0, y_sign * move_amt); 
                 (0, y_sign * (-1) * move_amt); (x_sign * (-1) * move_amt, 0)]
    | (0,m) -> [(0, y_sign * move_amt); (x_sign * move_amt, 0); 
                (x_sign * (-1) * move_amt,0); (0, y_sign * (-1) * move_amt)]
    | (n,m) -> [(x_sign * move_amt, 0); (0, y_sign * move_amt);
                (x_sign * (-1) * move_amt, 0); (0, y_sign * (-1) * move_amt);]
  in 
  helper_lst_maker pos_dif 

let rec helper_stop_following_move ghost user map = 
  let prev_move = Ghost.prev_move ghost in 
  let current_position = Ghost.get_position ghost in 
  let rec helper_find_dir prev_move current_position = 
    let dir =  Random.self_init (); Random.int 4 |> rand_char |> parse_dir in 
    if prev_move <> dir && Map.check_move current_position map dir true
    then Ghost.move ghost dir 
    else helper_find_dir prev_move current_position
  in 
  helper_find_dir prev_move current_position

let helper_make_aimed_move ghost user map rev =
  let move_lst = helper_possible_moves ghost user rev in 
  let rec move_lst_iter lst = 
    while not (made_move ghost) do 
      match lst with  
      | [] -> Ghost.move ghost (0,0)
      | h::t -> begin if Map.check_move (Ghost.get_position ghost) map h true
          then Ghost.move ghost h
          else move_lst_iter t 
        end 
    done
  in 
  move_lst_iter move_lst


(** [move_ghost_following] moves the ghost to follow the player if the timer 
    isn't up and the move is possible. The move is determined by 
    [helper_make_aimed_move] *)
let move_ghost_following ghost user map =
  if Ghost.is_following ghost 
  then begin 
    if following_counter ghost < int_of_float max_follow_time 
    then begin 
      Ghost.incr_following_count ghost;
      helper_make_aimed_move ghost user map false
    end 
    else helper_stop_following_move ghost user map 
  end 
  else begin 
    Ghost.start_following ghost; 
    helper_make_aimed_move ghost user map false
  end


(** [move_ghost_normal] is [move_ghost_following] when the ghost is near the 
    player or [move_ghost_prev] otherwise. *)
let move_ghost_normal ghost user map = 
  if are_close ghost user 
  then move_ghost_following ghost user map 
  else move_ghost_prev ghost map 


(** [move_ghost_reversed] is  [helper_make_aimed_move] when the ghost is near 
    the player or [move_ghost_prev] otherwise. *)
let move_ghost_reversed state ghost user map = 
  if state.reversal_timer <= int_of_float max_role_rev_time 
  then 
    begin 
      state.reversal_timer <- state.reversal_timer + 1;
      if are_close ghost user 
      then helper_make_aimed_move ghost user map true
      else move_ghost_prev ghost map 
    end 
  else 
    begin 
      state.reversal_timer <- 0;
      state.role_reversed <- false;
      move_ghost_normal ghost user map
    end 

let helper_move_regular state ghost map user = 
  reset_move ghost; 
  if state.role_reversed 
  then move_ghost_reversed state ghost user map 
  else move_ghost_normal ghost user map 

let helper_move_initial state ghost user map i = 
  let all_moves = Map.initial_ghost_moves map in 
  let current_moves = all_moves.(i) in 
  let move_counter = Ghost.init_counter ghost in 
  let move_index = move_counter/5 in 
  let current_position = Ghost.get_position ghost in 
  if move_index < (Array.length current_moves)
  then 
    begin 
      let dir = current_moves.(move_index) in  
      if Map.check_move current_position map dir false
      then Ghost.move_init ghost dir 
      else helper_move_regular state ghost map user
    end 
  else 
    begin 
      Ghost.finish_initializing ghost; 
      helper_move_regular state ghost map user
    end 

(** [move_ghosts] uses helper functions to determine and move the ghost 
    according to the current situation in the game. *)
let move_ghosts state ghosts map (user : Player.t) = 
  let g_counter = ref 0 in 
  Array.iter (fun g ->
      if is_done_initializing g
      then helper_move_regular state g map user
      else 
        begin 
          let num_g = state.num_ghosts - 1 in 
          helper_move_initial state g user map !g_counter; 
          g_counter := (!g_counter + 1) mod num_g
        end 
    )
    ghosts 

(** [flush] clears the user's inputs. *)
let flush () = 
  while Graphics.key_pressed () do 
    ignore (Graphics.read_key());
  done;
  ()

(** [pick_move] is the viable move that the player can make given their current
    command and their previous move. *)
let pick_move (user : Player.t) (map: Map.t) (next: point) (prev: point) 
    (prev_attempt: point) = 
  let user_pos = Player.get_position user in 
  if Map.check_move user_pos map next true && next <> (0, 0) then next 
  else if Map.check_move user_pos map prev_attempt true && prev_attempt <> (0,0) 
  then prev_attempt
  else if Map.check_move user_pos map prev true then prev 
  else (0,0)

let draw_ghosts ghosts = 
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
        draw_helper (x, y) t
      end
  in 
  draw_helper (100, 60) (lives_img_lst state) 

let move_player user map key = 
  let prev_move = player_prev_move user in 
  let next_move = parse_dir key in 
  let prev_attempt = player_prev_attempt user in
  let current_move = pick_move user map next_move prev_move prev_attempt in 
  Player.move user current_move;
  move_attempt user next_move

let draw_game (state: t) (display_player: bool)  = 
  draw_current_map state.map state.map_background;
  draw_lives state;
  if display_player then draw_player state.player else ();
  draw_ghosts state.ghosts

let map_init (map: Map.t): Graphics.image = 
  draw_map map;
  Graphics.get_image 0 0 window_width window_height 

let init_level (map_name: string) (fruit: fruit) (num_ghosts: int): t =
  let make_level map_name =
    let map = make_map (100, 100) map_name fruit in
    generate_special map; 
    let map_background = map_init map in
    let player = new_player () in 
    let ghosts = make_ghosts_helper num_ghosts map in
    initial_state player map ghosts map_background map_name
  in make_level map_name

let update_active (state: t) (key: char) : t = 
  let user = state.player in 
  let map = state.map in 
  let ghosts = state.ghosts in
  move_player user map key;  
  move_ghosts state ghosts map user; 
  let state' = update_state state in
  check_food (Player.get_position user) map;
  state'

let reset_level (state: t) : t = 
  if state.lives = 1 then 
    {state with game_state = Ended; lives = 0}
  else begin 
    Unix.sleep(1);
    {state with game_state = Active; 
                ghosts = make_ghosts_helper state.num_ghosts state.map;
                player = new_player();
                lives = state.lives - 1}
  end

let update_dying (state: t) : t =
  if death_ended state.player 
  then reset_level state 
  else begin 
    animate_death state.player;
    state
  end

let update_waiting (state: t) : t = 
  Unix.sleep(1);
  let user' = start_death state.player in 
  if state.food_left = 0 then state
  else {state with game_state = Dying; player = user'}

let update_ended (state: t) : t = 
  state

let update_level (state: t) (key: char) : t = 
  match state.game_state with 
  | Active -> update_active state key 
  | Dying -> update_dying state
  | Waiting -> update_waiting state
  | Ended -> update_ended state

let check_visibility (state: t) : bool =
  match state.game_state with 
  | Active | Waiting -> true
  | Dying -> 
    if Player.death_ended state.player then false else true
  | Ended -> false

let check_win (state: t) : int = 
  if state.food_left = 0 then 1 
  else if state.lives = 0 then -1 
  else 0
