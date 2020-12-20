open Graphics
open Graphic_image
open Images
open Map
open Player
open State
open Ghost
open Sprite
open Constants

(** The type [state] represents a game application state. *) 
type state = Loading | Active | Paused | Waiting | Win | Lose 

(** The [key] type represents a user keypress [k] with [Key k] and [None] 
    if there is no keypress. *) 
type key = None | Key of char

type game = {
  level: int;
  current: State.t; 
  state: state;
  prev_key: key;
  points: int;
  mutable fruit_basket: fruit array;
}

(** [cherry, strawberry, peach, apple, grapes] respresents each fruit
    to be included in the game. *)
let cherry = 
  let img = sprite_from_sheet sprite_sheet 2 3 fruit_width fruit_height 2 in 
  {sprite = img; points = 100}

let strawberry = 
  let img = sprite_from_sheet sprite_sheet 3 3 fruit_width fruit_height 2 in 
  {sprite = img; points = 300}

let peach = 
  let img = sprite_from_sheet sprite_sheet 4 3 fruit_width fruit_height 2 in 
  {sprite = img; points = 500}

let apple = 
  let img = sprite_from_sheet sprite_sheet 5 3 fruit_width fruit_height 2 in 
  {sprite = img; points = 700}

let grapes = 
  let img = sprite_from_sheet sprite_sheet 6 3 fruit_width fruit_height 2 in 
  {sprite = img; points = 1000}

(** [fruits] is the array containing the fruits to be included in the game. *)
let fruits = [|cherry; strawberry; peach; apple; grapes;|]

(** [maps] is the list of maps in the game. *)
let maps = [|"standard"; "OCaml"; "3110"|]

(** [fruit_num] is the number of total fruits in the game. *)
let fruit_num = Array.length fruits

let window_init (settings: string) : unit = 
  open_graph settings;
  set_window_title "Pacman";
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ())

(** [select_ghosts level] is the number of ghosts to be included based on the 
    level the player is in. *)
let select_ghosts (level: int) = 
  if level < 4 then level + 1 else 4

let init_game (map_name: string) (points: int) (level: int) 
    (fruit_basket: fruit array) (next_fruit: int) (lives: int): game = 
  let fruit_index = min next_fruit (fruit_num - 1) in
  let fruit = fruits.(fruit_index) in
  let level' = level + 1 in
  let ghost_num = select_ghosts level' in
  {level = level';
   current = init_level map_name fruit ghost_num lives; 
   state = Active; 
   prev_key = None; 
   points = points; 
   fruit_basket = fruit_basket;}

(** [check_key key_char] checks if the given char [key_char] is a valid 
    movement. *)
let check_key (key_char: char) : bool = 
  match key_char with 
  | 'a' | 'w' | 's' | 'd' -> true 
  | _ -> false

(** [check_space game key key_char] checks if the [key] entered is a space. 
    If it is, the [game] is set to paused. If not, the next key [key_char] is 
    checked, and if it is valid, then the old [key] is set to be the previous
    key. Otherwise, the current game is returned. *)
let check_space (game: game) (key: key) (key_char: char) : game = 
  if key = Key ' ' && not (game.prev_key = Key ' ')
  then {game with state = Paused; prev_key = key}
  else if check_key key_char 
  then {game with prev_key = key}
  else game

(** [update_active_game game key key_char level] updates the [game] state and 
    sets the previous key as [key] based on the winning status and current 
    [level]. If the player did not win or lose, it checks for the next key, 
    [key_char] *)
let update_active_game (game: game) (key: key) (key_char: char) 
    (level: State.t) : game =
  let win_code = State.check_win level in
  let game' = {game with current = level} in
  if win_code = 1 
  then {game' with state = Win; prev_key = key} 
  else if win_code = -1
  then {game' with state = Lose; prev_key = key} 
  else check_space game' key key_char

(** [check_fruits game level] checks if there are still fruits that need to be
    given in a level of the [game] based on the current [level]. *)
let check_fruits (game: game) (level: State.t) : unit =
  if (fruit_eaten level) && not (fruit_eaten game.current) 
  then begin 
    let next_fruit = Array.length game.fruit_basket in
    if next_fruit < fruit_num then begin
      let fruit = fruits.(next_fruit) in
      game.fruit_basket <- Array.append game.fruit_basket [|fruit|] 
    end
  end

(** [update_active game key] updates the [game] based on the next [key] entered
    by the player. *)
let update_active (game: game) (key: key) : game = 
  let key_char = 
    match key with 
    | Key k -> k 
    | None -> 'z'
  in
  let level' = update_level game.current key_char in 
  check_fruits game level';
  if (lives game.current) > (lives level') 
  then {game with current = level'; 
                  prev_key = None; 
                  state = Waiting}
  else update_active_game game key key_char level'

(** [update_paused game key] updates the [game] to be paused or active based on
    the [key]. *)
let update_paused (game: game) (key: key) : game = 
  if key = Key ' ' && not (game.prev_key = Key ' ')
  then {game with state = Active; prev_key = None} 
  else {game with prev_key = key}

(** [update_win game key] updates the window and [game] state if the player
    won a level. *)
let update_win (game: game) (key: key) : game = 
  Unix.sleep(1);
  animate_win game.current;
  {game with state = Loading}

(** [update_lose game] returns the game if the player lost. *)
let update_lose (game: game) : game =
  game

(** [update_loading game] sets the parameters of [game] to be ready for the next
    level.  *)
let update_loading (game: game) : game = 
  clear_graph();
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ());
  let points = game.points + points game.current in
  let fruits = game.fruit_basket in
  let next_fruit = Array.length fruits in
  let map = Random.self_init (); Random.int (Array.length maps) in
  let lives = lives game.current + 1 in
  let game' = init_game maps.(map) points game.level fruits next_fruit lives in 
  {game' with state = Waiting}

(** [update_waiting game] updates the [game] to active after loading the next 
    level. *)
let update_waiting (game: game) : game = 
  Unix.sleep(2);
  {game with state = Active}


(** [draw_labels game] draws the number of points and level of the [game] in the
    window. *)
let draw_labels (game: game) : unit = 
  set_color red;
  moveto 175 675;
  let points = game.points + (points game.current) in
  draw_string ("Points: " ^ string_of_int points);
  moveto 1275 675;
  draw_string ("Level: " ^ string_of_int game.level)

(** [draw_fruits game] draws the fruits on the window during the [game] *)
let draw_fruits (game: game) : unit = 
  let draw_helper x y index fruit : unit = 
    let x = x - 50 * index in
    let img = fruit.sprite |> sprite_image |> Graphic_image.of_image in 
    Graphics.draw_image img x y 
  in 
  ignore (Array.mapi (draw_helper 1275 60) game.fruit_basket);
  ()

(** [draw_end_game] draws the end game screen. *)
let draw_end_game : unit = 
  let image = Png.load_as_rgb24("./sprites/end_game_screen.png") [] in
  let g = Graphic_image.of_image image in
  Graphics.draw_image g 500 300

(** [draw game] draws the [game], fruits, labels, or the end game screen if the
    player lost.  *)
let draw (game: game) : unit =
  let level = game.current in
  draw_game level (check_visibility level);
  draw_labels game;
  draw_fruits game;
  if game.state = Lose then draw_end_game

(** [update game] updates the [game] based on its status through calling the 
    respective function.  *)
let rec update (game: game) : unit = 
  let key = 
    if Graphics.key_pressed() 
    then Key (Graphics.read_key())
    else None
  in
  let game' = 
    match game.state with 
    | Active -> update_active game key
    | Paused -> update_paused game key
    | Win -> update_win game key 
    | Lose -> update_lose game
    | Loading -> update_loading game
    | Waiting -> update_waiting game
  in 
  if game'.state <> Loading then draw game';
  (* Graphics.draw_image (pts_1000 |> sprite_image |> Graphic_image.of_image) 25 
     25; *)
  synchronize ();
  Unix.sleepf(sleep_time); 
  update game'

let main (settings: string) : unit = 
  window_init settings;
  auto_synchronize false;
  let game = init_game "standard" 0 0 [||] 0 3 in
  ignore (update game);
  ()

(** () sets the window settings and calls the main to start the game. *)
let () = 
  let settings = 
    string_of_int window_width ^ "x" ^ string_of_int window_height
  in
  main (" " ^ settings) 

