open Graphics
open Graphic_image
open Images
open Map
open Player
open State
open Ghost
open Sprite
open Constants

type state = Loading | Active | Paused | Win | Lose

type key = None | Key of char

type game = {
  level: int;
  current: State.t; 
  state: state;
  prev_key: key;
  prev_move: char;
  points: int;
}

let tile_type str = 
  ("Tile type: " ^ str)

let food_count food = 
  ("Food Count: " ^ string_of_int food)

let check_move b = 
  ("Can Move: " ^ string_of_bool b)

let window_init (settings: string) : unit = 
  open_graph settings;
  set_window_title "Pacman";
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ())

let init_game (map_name: string) (points: int) (level: int) : game = 
  {level = level + 1;
   current = init_level map_name; 
   state = Active; 
   prev_key = None; 
   prev_move = 'z';
   points = points}

let update_active_game (game: game) (key: key) (key_char: char) 
    (level: State.t) : game =
  let win_code = State.check_win level in
  if win_code = 1 
  then {game with current = level; prev_key = key; state = Win} 
  else if win_code = -1
  then {game with current = level; prev_key = key; state = Lose} 
  else 
    begin
      if key = Key ' ' && not (game.prev_key = Key ' ')
      then {game with current = level; prev_key = key; state = Paused}
      else {game with current = level; prev_key = key; prev_move = key_char}
    end

let update_active (game: game) (key: key): game = 
  let key_char = 
    match key with 
    | Key k -> k 
    | None -> game.prev_move 
  in
  let level' = update_level game.current key_char in 
  update_active_game game key key_char level'

let update_paused (game: game) (key: key) : game = 
  if key = Key ' ' && not (game.prev_key = Key ' ')
  then {game with state = Active; prev_key = key} 
  else {game with prev_key = key}

let update_win (game: game) (key: key) : game = 
  if key = Key ' ' && not (game.prev_key = Key ' ')
  then {game with state = Loading; prev_key = key} 
  else {game with prev_key = key}

let update_lose (game: game) : game =
  failwith "unimplemented"

let update_loading (game: game) : game = 
  clear_graph();
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ());
  let points = game.points + points game.current in
  init_game "OCaml" points game.level 

let draw_labels (game: game) : unit = 
  set_color red;
  moveto 175 75;
  let points = game.points + (points game.current) in
  draw_string ("Points: " ^ string_of_int points);
  moveto 175 675;
  draw_string ("Level: " ^ string_of_int game.level)

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
  in 
  draw_labels game';
  synchronize ();
  Unix.sleepf(sleep_time); 
  update game'

let main (settings: string) : unit = 
  window_init settings;
  auto_synchronize false;
  let game = init_game "OCaml" 0 0 in
  ignore (update game);
  ()

let () = 
  let settings = 
    string_of_int window_width ^ "x" ^ string_of_int window_height
  in
  main (" " ^ settings) 
