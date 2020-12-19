open Graphics
open Graphic_image
open Images
open Map
open Player
open State
open Ghost
open Sprite
open Constants

type state = Loading | Active | Paused | Waiting | Win | Lose 

type key = None | Key of char

type game = {
  level: int;
  current: State.t; 
  state: state;
  prev_key: key;
  points: int;
  mutable fruit_basket: fruit array;
}

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
  let img = sprite_from_sheet sprite_sheet 5 4 fruit_width fruit_height 2 in 
  {sprite = img; points = 900}

let fruits = [|cherry; strawberry; peach; apple; grapes|]

let maps = [|"standard"; "OCaml"; "3110"|]

let fruit_num = Array.length fruits

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

let select_ghosts (level: int) = 
  if level < 4 then level + 1 else 4

let init_game (map_name: string) (points: int) (level: int) 
    (fruit_basket: fruit array) (next_fruit: int) (lives: int): game = 
  let fruit = 
    if next_fruit = fruit_num 
    then fruits.(fruit_num - 1) 
    else fruits.(next_fruit)
  in
  let level' = level + 1 in
  (* let level' = 3 in  
     let map_name = "OCaml" in *)
  let ghost_num = select_ghosts level' in
  {level = level';
   current = init_level map_name fruit ghost_num lives; 
   state = Active; 
   prev_key = None; 
   points = points;
   fruit_basket = fruit_basket;}

let check_key (key_char: char) : bool = 
  match key_char with 
  | 'a' | 'w' | 's' | 'd' -> true 
  | _ -> false

let check_space (game: game) (key: key) (key_char: char) : game = 
  if key = Key ' ' && not (game.prev_key = Key ' ')
  then {game with state = Paused; prev_key = key}
  else if check_key key_char 
  then {game with prev_key = key}
  else game

let update_active_game (game: game) (key: key) (key_char: char) 
    (level: State.t) : game =
  let win_code = State.check_win level in
  let game' = {game with current = level} in
  if win_code = 1 
  then {game' with state = Win; prev_key = key} 
  else if win_code = -1
  then {game' with state = Lose; prev_key = key} 
  else check_space game' key key_char

let check_fruits (game: game) (level: State.t) : unit =
  if (fruit_eaten level) && not (fruit_eaten game.current) 
  then
    begin 
      let next_fruit = Array.length game.fruit_basket in
      let fruit = fruits.(next_fruit) in
      game.fruit_basket <- Array.append game.fruit_basket [|fruit|] 
    end
  else ()

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

let update_paused (game: game) (key: key) : game = 
  if key = Key ' ' && not (game.prev_key = Key ' ')
  then {game with state = Active; prev_key = None} 
  else {game with prev_key = key}

let update_win (game: game) (key: key) : game = 
  Unix.sleep(1);
  {game with state = Loading}

let update_lose (game: game) : game =
  game

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

let update_waiting (game: game) : game = 
  Unix.sleep(2);
  {game with state = Active}

let draw_labels (game: game) : unit = 
  set_color red;
  moveto 175 675;
  let points = game.points + (points game.current) in
  draw_string ("Points: " ^ string_of_int points);
  moveto 1275 675;
  draw_string ("Level: " ^ string_of_int game.level)

let draw_fruits (game: game) : unit = 
  let draw_helper x y index fruit : unit = 
    let x = x - 50 * index in
    let img = fruit.sprite |> sprite_image |> Graphic_image.of_image in 
    Graphics.draw_image img x y 
  in 
  ignore (Array.mapi (draw_helper 1275 60) game.fruit_basket);
  ()

let draw_end_game (game: game) : unit = 
  let image = Png.load_as_rgb24("./sprites/end_game_screen.png") [] in
  let g = Graphic_image.of_image image in
  Graphics.draw_image g 500 300

let draw (game: game) : unit =
  let level = game.current in
  draw_game level (check_visibility level);
  draw_labels game;
  draw_fruits game;
  if game.state = Lose then draw_end_game game

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
  draw game';
  synchronize ();
  Unix.sleepf(sleep_time); 
  update game'

let main (settings: string) : unit = 
  window_init settings;
  auto_synchronize false;
  let game = init_game "standard" 0 1 [||] 0 3 in
  ignore (update game);
  ()

let () = 
  let settings = 
    string_of_int window_width ^ "x" ^ string_of_int window_height
  in
  main (" " ^ settings) 

