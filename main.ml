open Graphics
open Graphic_image
open Images
open Map
open Player
open State
open Ghost
open Sprite
open Constants

type state = Active | Paused

type key = None | Key of char

type game = {
  level: State.t; 
  state: state;
  prev_key: key;
  prev_move: char;
}

let tile_type str = 
  ("Tile type: " ^ str)

let food_count food = 
  ("Food Count: " ^ string_of_int food)

let check_move b = 
  ("Can Move: " ^ string_of_bool b)

let set_window (title: string) (color: Graphics.color) : unit = 
  set_window_title title;
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ())

let window_init (settings: string) : unit = 
  open_graph settings;
  set_window "Pacman" black;
  set_color black;
  fill_rect 0 0 window_width window_height

let rec update (game: game) : unit = 
  let key = 
    if Graphics.key_pressed() 
    then Key (Graphics.read_key())
    else None
  in
  match game.state with 
  | Active -> update_active game key
  | Paused -> update_paused game key

and update_active (game: game) (key: key): unit = 
  let key_char = 
    match key with 
    | Key k -> k 
    | None -> game.prev_move 
  in
  let level' = update_level game.level key_char in 
  let game' = 
    if key = Key ' ' && not (game.prev_key = Key ' ')
    then {game with level = level'; prev_key = key; state = Paused}
    else {game with level = level'; prev_key = key; prev_move = key_char}
  in 
  synchronize ();
  Unix.sleepf(sleep_time); 
  update game'

and update_paused (game: game) (key: key) : unit = 
  let game' = 
    if key = Key ' ' && not (game.prev_key = Key ' ')
    then {game with state = Active; prev_key = key} 
    else {game with prev_key = key}
  in
  Unix.sleepf(sleep_time); 
  update game'

let init_game (map_name: string) : game = 
  {level = init_level map_name; 
   state = Active; 
   prev_key = None; 
   prev_move = 'z'}

let main (settings: string) : unit = 
  window_init settings;
  auto_synchronize false;
  let game = init_game "OCaml" in
  ignore (update game);
  ()

let () = 
  let settings = 
    string_of_int window_width ^ "x" ^ string_of_int window_height
  in
  main (" " ^ settings) 

(* synchronize ();
   Unix.sleepf(sleep_time); 
   loop state' map_image  *)