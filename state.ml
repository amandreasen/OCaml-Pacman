open Ghost
open Map
open Player
open Sprite
open Constants
open Graphics 
open Graphic_image

(**[game_state] represents a state that a game level can be in. An [Active] 
   level is played normally, with ghost and player movement enabled. A [Waiting]
   will pause for a short amount of time, during which no movement can occur. 
   A [Dying] level will not display the ghost and will animate a player death, 
   during which the player cannot be moved. An [Ended] level does not display 
   the player or ghosts, and no further action can be taken. *) 
type game_state = Active | Waiting | Dying | Ended

(**A [background] contains images of a map drawn in both a blue and white 
   outline. *) 
type background = {
  white: Graphics.image;
  blue: Graphics.image
}

type t = {
  map_name: string;
  player : Player.t;
  points : int;
  lives : int;
  ghosts : Ghost.t array;
  num_ghosts : int;
  map : Map.t;
  map_backgrounds: background;
  follower_ghosts : Ghost.t list;
  food_left: int;
  fruit_generated: bool;
  fruit_active: bool;
  fruit_eaten: bool;
  mutable fruit_timer: int;
  game_state: game_state;
  mutable role_reversed: bool;
  mutable reversal_timer : int;
  ghosts_eaten: int
}

let points state =
  state.points

let lives state =
  state.lives

let fruit_eaten state =   
  state.fruit_eaten

(**[initial_state player map ghosts_entry backgrounds map_name lives] 
   will initialize a state record with the player [player], map [map], 
   ghost list [ghosts_entry], the map images [backgrounds], the map name 
   [map_name], and the number of lives [lives]. *) 
let initial_state player map ghosts_entry backgrounds map_name lives = {
  map_name = map_name;
  player = player;
  points = 0;
  lives = lives;
  ghosts = ghosts_entry;
  num_ghosts = Array.length ghosts_entry;
  map = map;
  map_backgrounds = backgrounds;
  follower_ghosts = [];
  food_left = food_count map;
  fruit_generated = false;
  fruit_active = false;
  fruit_eaten = false;
  fruit_timer = 0;
  game_state = Active;
  role_reversed = false;
  reversal_timer = 0;
  ghosts_eaten = 0
} 

(**[update_state_food value state] will update the points and available 
   fruit in [state] using the point value [value] to determine which tile the
   player interacted with in the current frame. *) 
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
  if food_left = fruit_limit && not state.fruit_generated then begin 
    generate_fruit state.map;
    {state' with fruit_generated = true;
                 fruit_active = true;
                 fruit_timer = fruit_timer;}
  end 
  else state'

(**[check_overlap user_pos ghost] will return true if the distance between the 
   player position [user_pos] and the center of the ghost [ghost] are less 
   than a predetermined threshold amount, and false otherwise. *) 
let check_overlap user_pos ghost =
  let ghost_pos = Ghost.get_position ghost in
  let ghost_x = fst ghost_pos in 
  let ghost_y = snd ghost_pos in 
  let user_x = fst user_pos in 
  let user_y = snd user_pos in 
  let threshold = png_wl / 2 in
  (Int.abs (ghost_x - user_x) <= threshold && user_y = ghost_y) || 
  (Int.abs (ghost_y - user_y) <= threshold && user_x = ghost_x)

(** The type [overlap] will be [None] if there is no overlap between a ghost 
    and a player and [Ghost g] if the player overlaps with ghost [g]. *) 
type overlap = None | Ghost of Ghost.t

(** [get_overlap user_pos ghosts] will return an [overlap] type. It will 
    return [Ghost g] if user_pos overlaps with a ghost in [ghosts], where [g]
    is the first ghost in [ghosts] with a player overlap. Otherwise, it will
    return None. *) 
let get_overlap (user_pos: int * int) (ghosts: Ghost.t array) : overlap = 
  let overlap = ref None in 
  let get_overlap acc g = 
    if !acc <> None then acc 
    else if check_overlap user_pos g then ref (Ghost g)
    else ref None
  in 
  !(Array.fold_left get_overlap overlap ghosts)

(**[reset_roles] will reset a level [state] with role reversal enabled 
   to not have role reversal enabled. *) 
let reset_roles (state: t) : t = 
  ignore (Array.map (fun g -> set_state g "active") state.ghosts);
  {state with role_reversed = false; reversal_timer = 0; ghosts_eaten = 0}

(**[update_special_food state pts] will update [state] to have role reversal 
   enabled if [pts] is equal to the amount of points a special food pellet is 
   worth. Otherwise, if role reversal is currently enabled, the function 
   will reset [state] to not have role reversal enabled if the timer for role
   reversal is exceeded. If role reversal is not currently enabled and a 
   special food has not been consumed, the function will not update state. *) 
let update_special_food state pts = 
  if pts = special_val 
  then begin 
    ignore (Array.map (fun g -> set_state g "scared1") state.ghosts);
    {state with role_reversed = true; reversal_timer = 1} 
  end
  else begin 
    if state.reversal_timer >= max_role_rev_time 
    then reset_roles state
    else state
  end 

(**[get_ghost_value ghosts_eaten] will return the appropriate number of points
   for a eaten ghost depending on the number of ghosts already eaten while 
   this role reversal was active [ghosts_eaten]. *) 
let get_ghost_value (ghosts_eaten: int) : int = 
  match ghosts_eaten with 
  | 1 -> 200 
  | 2 -> 400 
  | 3 -> 800 
  | 4 -> 1600
  | _ -> 1600

(**[update_eaten state ghost] will update the level [state] after a ghost has 
   been eaten. It will also update the eaten ghost [ghost]. *) 
let update_eaten (state: t) (ghost: Ghost.t) : t = 
  match get_state ghost with 
  | "eaten" -> state
  | _ -> 
    set_state ghost "eaten"; 
    let eaten = min (state.ghosts_eaten + 1) 4 in 
    let points = get_ghost_value eaten in
    {state with ghosts_eaten = eaten; points = state.points + points}

(** [update_game_state state map] will return an updated level that represents
    the level [state] with the map [map] after ghost-player overlaps have been 
    checked.*) 
let update_game_state state map = 
  let player_pos = Player.get_position state.player in
  let ghosts = state.ghosts in
  let overlapped_ghost = get_overlap player_pos ghosts in 
  match overlapped_ghost with 
  | None -> state
  | Ghost g -> 
    if state.role_reversed then update_eaten state g
    else begin 
      Player.reset_move state.player;
      {state with game_state = Waiting; ghosts = [||]} 
    end

(** [update_ghosts state] will update scared blue ghosts to be scared 
    white ghosts at the threshold time in the level [state]. *)
let update_ghosts (state: t) : unit = 
  let threshold_time = max_role_rev_time - 25 in 
  let update_scared ghost = 
    let ghost_state = get_state ghost in
    match ghost_state with 
    | "eaten" | "active" -> ()
    | _ -> set_state ghost "scared2"
  in
  if state.reversal_timer = threshold_time
  then ignore (Array.map update_scared state.ghosts); 
  ()

(** [update_timer state] will update the role reversal timer in level [state]
    and disable role reversal if the time limit has been reached. *) 
let update_timer (state: t) : unit = 
  if state.reversal_timer <= max_role_rev_time && state.reversal_timer > 0 
  then state.reversal_timer <- state.reversal_timer + 1
  else begin 
    state.reversal_timer <- 0;
    state.role_reversed <- false
  end 

(**[update_state] will return an updated level representing the updated level 
   [state].*) 
let update_state (state: t) : t = 
  let player_pos = Player.get_position state.player in 
  let point_val = Map.get_tile_value player_pos state.map in
  let state' = update_game_state state state.map 
               |> update_state_food point_val 
  in 
  if state'.food_left = 0 then Player.reset_move state.player;
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

(** [new_follower state ghost] is [state] with a new following ghost [ghost]. *)
let new_follower state ghost = 
  {state with follower_ghosts = ghost :: state.follower_ghosts}

(** [remove_follower state ghost] is [state] the following ghost [ghost] 
    removed. *)
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

(** [make_ghosts_helper num map] is the ghost array of [num] ghosts. Initial 
    positions of the ghosts are determined based on [map]'s initial ghost 
    position array. *)
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
  let ghost_arr = !acc |> List.rev |> Array.of_list  in 
  ghost_arr

let life_image = sprite_from_sheet sprite_sheet 8 1 50 50 2

(** [lives_img_lst state] is the Sprite.t list of the number of lives the user
    has left. *)
let lives_img_lst state = 
  List.init state.lives (fun x -> life_image)

(** [parse_dir dir] is the tuple representing the change in coordinates from the 
    user's character input. *)
let parse_dir (dir: char) =
  match dir with 
  |'w' ->  (0, move_amt)
  |'s' -> (0, -move_amt)
  |'a' -> (-move_amt, 0)
  |'d' -> (move_amt, 0)
  | _ -> (0,0)

(** [rand_char num] is the char for a movement as determined by [num]. For  
    ghost movement, [num] is a random int in the range [0,3]. *)
let rand_char num = 
  match num with 
  | 0 -> 'w'
  | 1 -> 's'
  | 2 -> 'a'
  | 3 -> 'd'
  | _ -> ' '

(** [number_sign n] is 1 if n is nonnegative and -1 if n is negative. *)
let number_sign n = 
  match n with 
  | n when n >= 0 -> 1
  | _ -> -1

(** [are_close ghost user] is true if the distance between the [ghost] and 
    [user] are less than or equal to the [closeness_threshold]. *)
let are_close ghost user  = 
  let x_g = fst (Ghost.get_position ghost) in 
  let x_u = fst (Player.get_position user) in 
  let y_g = snd (Ghost.get_position ghost) in 
  let y_u = snd (Player.get_position user) in 
  let x_squared = (x_g + x_u) * (x_g + x_u) in 
  let y_squared = (y_g + y_u) * (y_g + y_u) in 
  let distance = sqrt (float_of_int (x_squared - y_squared)) in 
  distance <= closeness_threshold

(** [position_diff ghost user rev] is the [user] position minus the [ghost] 
    position if rev is true, or the [ghost] position minus the [user] position 
    otherwise. *)
let position_diff ghost user rev = 
  let x_g = fst (Ghost.get_position ghost) in 
  let x_u = fst (Player.get_position user) in 
  let y_g = snd (Ghost.get_position ghost) in 
  let y_u = snd (Player.get_position user) in 
  if rev then (x_g - x_u, y_g - y_u)
  else (x_u - x_g, y_u - y_g)

(** [move_ghost_randomly ghost map] selects a random direction and if [ghost] 
    can move in that direction in [map] then the [ghost] makes this move; 
    otherwise, a new random direction is tried until a move can be made. *)
let move_ghost_randomly ghost map = 
  let start = Random.self_init (); Random.int 4 in 
  let next = ref start in 
  let dir = !next |> rand_char |> parse_dir |> ref in 
  while not (Map.check_move (Ghost.get_position ghost) map !dir true) do 
    next := (!next + 1) mod 4;
    dir := !next |> rand_char |> parse_dir;
  done;
  Ghost.move ghost !dir 

(**[move_selector map ghost_pos dir] will try to select a new valid move 
   that can be made from position [ghost_pos] in the map [map] with 
   an opposite orientation to the current direction [dir]. If not valid 
   move can be selected, the function returns [dir]. *) 
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

(**[select_move_new map ghost_pos dir] will randomly attempt to select
   either a move in the current direction [dir] or a new valid move that can 
   be made from [ghost_pos] in the map [map]. *) 
let select_move_new (map: Map.t) (ghost_pos: int * int) (dir: int * int) 
  : int * int = 
  let select = Random.self_init(); Random.int 2 in 
  if select = 0 then dir 
  else move_selector map ghost_pos dir

(**[move_ghost_new ghost map] will make either an algorithmically selected 
   move for [ghost] or a randomly selected move if the previous selected move 
   would not be a valid move in [map]. *) 
let move_ghost_new ghost map = 
  let dir = Ghost.prev_move ghost in 
  let ghost_pos = Ghost.get_position ghost in 
  let new_move = select_move_new map ghost_pos dir in 
  if Map.check_move ghost_pos map new_move true
  then Ghost.move ghost new_move
  else move_ghost_randomly ghost map 

(** [helper_possible_moves ghost user rev] is the list of possible moves that 
    [ghost] could make depending on the [ghost] and [user] positions, and 
    whether the game is in a reversed state, as determined by [rev]. *)
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

(** [helper_stop_following_move ghost user map] moves [ghost] in [map] in a way 
    such that it is no longer following [user]. *)
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

(** [helper_make_aimed_move ghost user map rev] moves [ghost] in [map] in a way 
    such that if rev is true then the ghost would be moving away from [user], if 
    possible, and if rev is false then the ghost would be moving towards [user], 
    if possible in [map]. *)
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

(** [move_ghost_following ghost user map] moves [ghost] to follow [player] 
    if the following timer isn't up and the move is possible in [map]. *)
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


(** [move_ghost_normal ghost user map] is [move_ghost_following ghost user map] 
    when [ghost] is near [player] or [move_ghost_prev ghost map] otherwise. *)
let move_ghost_normal ghost user map = 
  if are_close ghost user 
  then move_ghost_following ghost user map 
  else move_ghost_new ghost map 

(** [move_ghost_reversed state ghost user map] moves [ghost] away from [user]
    if the game is in reversed state or moves [ghost] normally otherwise. *)
let move_ghost_reversed state ghost user map = 
  if state.reversal_timer > 0
  then begin 
    if are_close ghost user 
    then helper_make_aimed_move ghost user map true
    else move_ghost_new ghost map 
  end 
  else move_ghost_normal ghost user map

(** [helper_move_regular state ghost map user] moves [ghost] after it has been
    initialized. If the game is in reversed state then [ghost] tries to move
    away from [user] or [ghost] tries a normal move otherwise. *)
let helper_move_regular state ghost map user = 
  Ghost.reset_move ghost; 
  if state.role_reversed 
  then move_ghost_reversed state ghost user map 
  else move_ghost_normal ghost user map 

(** [helper_move_initial state ghost user map i] moves [ghost] in [map] 
    according to the initial moves determined in [map]. If [ghost] has completed
    it's initial moves, meaning it has left the ghost box or original position, 
    then [ghost] moves normally in [map]. *)
let helper_move_initial state ghost user map i = 
  let all_moves = Map.initial_ghost_moves map in 
  let current_moves = all_moves.(i) in 
  let move_counter = Ghost.init_counter ghost in 
  let move_index = move_counter / 5 in 
  let current_position = Ghost.get_position ghost in 
  if move_index < (Array.length current_moves)
  then begin 
    let dir = current_moves.(move_index) in  
    if Map.check_move current_position map dir false
    then Ghost.move_init ghost dir 
    else helper_move_regular state ghost map user
  end 
  else begin 
    Ghost.finish_initializing ghost; 
    helper_move_regular state ghost map user
  end 

(** [move_ghosts] uses helper functions to determine and move the ghost 
    according to the current situation in the game. *)
let move_ghosts state ghosts map (user : Player.t) = 
  for i = 0 to state.num_ghosts - 1 do 
    let g = ghosts.(i) in 
    if is_done_initializing g
    then helper_move_regular state g map user
    else helper_move_initial state g user map i 
  done 


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
  let try_prev = 
    if Map.check_move user_pos map prev true then prev 
    else (0,0)
  in 
  let try_prev_attempt = 
    if Map.check_move user_pos map prev_attempt true && prev_attempt <> (0,0) 
    then prev_attempt
    else try_prev
  in 
  let try_next = 
    if Map.check_move user_pos map next true && next <> (0, 0) then next 
    else try_prev_attempt
  in 
  try_next 

(**[draw_ghosts ghosts] will draw the list of ghosts [ghosts] to the 
   current open GUI window. *) 
let draw_ghosts ghosts = 
  let draw_helper ghost =
    let pos = Ghost.get_position ghost in 
    let x = fst pos in 
    let y = snd pos in 
    let image = ghost
                |> get_sprite 
                |> sprite_image 
                |> Graphic_image.of_image 
    in 
    Graphics.draw_image image (x - ghost_radius) (y - ghost_radius)
  in 
  ignore (Array.iter draw_helper ghosts);
  ()

(** [draw_player user] will draw the player [user] to the current open GUI
    window. *) 
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

(** [draw_current_map map map_image] will draw the map background [map_image]
    and the food in the map [map] to the current open GUI window.  *) 
let draw_current_map (map: Map.t) (map_image: Graphics.image) = 
  Graphics.draw_image map_image 0 0;
  Map.draw_food map

(** [draw_lives state] will draw the current number of lives in [state] 
    as images to the current open GUI window in the bottom left corner of the
     map. *) 
let draw_lives state : unit = 
  let draw_helper x y index life = 
    let x_pos = x + 50 * index in 
    let img = life |> sprite_image |> Graphic_image.of_image in 
    Graphics.draw_image img x_pos y
  in 
  ignore (List.mapi (draw_helper 150 60) (lives_img_lst state)); 
  ()

(** [move_player user map key] moves [user]. The direction is determined by 
    trying to move [user] in the direction specified by the  keyboard input 
    [key], the previous keyboard input, the previous move made by [user], and if 
    all those fail, [user] does not make a move. *)
let move_player user map key = 
  let prev_move = player_prev_move user in 
  let next_move = parse_dir key in 
  let prev_attempt = player_prev_attempt user in
  let current_move = pick_move user map next_move prev_move prev_attempt in 
  Player.move user current_move;
  move_attempt user next_move

(** [draw_game state display_player] will draw the level [state] to the GUI
    window. The player is drawn only if [display_player] is true. *) 
let draw_game (state: t) (display_player: bool)  = 
  draw_current_map state.map state.map_backgrounds.blue;
  draw_lives state;
  if display_player then draw_player state.player else ();
  draw_ghosts state.ghosts

(** [map_init map color] will create a map image of the map [map] with the 
    wall color [color]. *) 
let map_init (map: Map.t) (color: Graphics.color): Graphics.image = 
  draw_map map color;
  Graphics.get_image 0 0 window_width window_height 

let init_level (map_name: string) (fruit: fruit) (num_ghosts: int) 
    (lives: int): t =
  let make_level map_name =
    let map = make_map (100, 100) map_name fruit in
    generate_special map; 
    let map_bg_white = map_init map Graphics.white in
    let map_bg_blue = map_init map wall_color in
    let bgs = {white = map_bg_white; blue = map_bg_blue} in
    let player = new_player () in 
    let ghosts = make_ghosts_helper num_ghosts map in
    initial_state player map ghosts bgs map_name lives
  in make_level map_name

(**[update_active state key] will update an active level [state] with the
   user keypress [key]. *) 
let update_active (state: t) (key: char) : t = 
  let user = state.player in 
  let map = state.map in 
  let ghosts = state.ghosts in
  update_timer state;
  move_player user map key;  
  move_ghosts state ghosts map user; 
  update_ghosts state;
  let state' = update_state state in
  check_food (Player.get_position user) map;
  state'

(**[reset_level state] will reset the current level by ending the game if
   the user has no lives remaining, or by resetting the user and ghost 
   positions and decrementing the number of lives. *) 
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

(**[update_dying state] will continue a user death animation in the level 
   [state] if the animation is not finished, or it will reset the level and 
   player if the animation has finished. *) 
let update_dying (state: t) : t =
  if death_ended state.player 
  then begin 
    Player.reset_move state.player;
    reset_level state 
  end
  else begin 
    animate_death state.player;
    state
  end

(** [update_waiting state] will pause the level [state] briefly before 
    transitioning to either a win animation or death animation, depending on the 
    current parameters of [state]. *) 
let update_waiting (state: t) : t = 
  Unix.sleep(1); 
  let user' = start_death state.player in 
  if state.food_left = 0 then state
  else {state with game_state = Dying; player = user'}

(** [update_ended state] will update a level [state] that has been ended. No
    movement is allowed. *) 
let update_ended (state: t) : t = 
  Player.reset_move state.player;
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

let animate_win (state: t) : unit =
  let counter = ref 0 in 
  let white_map = state.map_backgrounds.white in 
  let blue_map = state.map_backgrounds.blue in 
  while !counter < 8 do 
    let value = !counter in 
    if value mod 2 = 0 then Graphics.draw_image white_map 0 0
    else Graphics.draw_image blue_map 0 0;
    draw_player state.player;
    counter := !counter + 1;
    synchronize();
    Unix.sleepf(0.2)
  done