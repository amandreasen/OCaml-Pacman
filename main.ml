open Graphics
open Map
open Player
open State

let window_width = 1500
let window_height = 750
let map_width = 1300
let map_height = 550

let move_amt = 50

let game_status state = ("Points: " ^ string_of_int (points state)
                         ^ "   Lives: " ^ string_of_int (lives state)
                         ^ "   Level: " ^ string_of_int (current_level state))

let tile_type str = ("Tile type: " ^ str)

let food_count food = ("Food Count: " ^ string_of_int (food))

let check_move b = ("Can Move: " ^ string_of_bool(b))

let set_window (title: string) (color: Graphics.color) : unit = 
  set_window_title title;
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ())

let parse_dir (user: Player.t) (dir: char) =
  match dir with 
  |'w' ->  (0, move_amt)
  |'s' -> (0, -move_amt)
  |'a' -> (-move_amt, 0)
  |'d' -> (move_amt, 0)
  |_ -> (0,0)

let rec loop () user map state= 
  Unix.sleep(0);
  let create_sprite dir = 
    if (Map.check_move (get_position user) map dir) then begin
      Player.move user dir;
      clear_graph ();
      set_window "Pacman" black;
      set_color blue;
      check_food (get_position user) map;
      draw_map map;
      (*draw_image map_image 0 0;*)
      moveto 175 75;
      set_color red;
      let new_state = State.update_state_food state map in
      draw_string (game_status new_state);
      draw_string (tile_type (Map.get_tile_type (get_position user) map));
      (*draw_string (check_move (Map.check_move (get_position user) map dir));*)
      set_color yellow; 
      fill_circle (fst (get_position user)) (snd (get_position user)) 25; end
  in
  create_sprite (parse_dir user (Graphics.read_key ())); 
  loop () user map state

let main (settings: string) : unit = 
  open_graph settings;
  set_window "Pacman" black;
  set_color black;
  fill_rect 0 0 window_width window_height;
  (* set_color blue;
     draw_rect 100 100 map_width map_height; *)
  let map = make_map (100,100) "OCaml" in 
  draw_map map;
  let map_image = get_image 0 0 window_width window_height in 
  set_color yellow; 
  fill_circle 175 175 25;
  moveto 175 75;
  let state = initial_state map [] in 
  set_color red;
  draw_string (game_status state);
  ignore (loop () new_player map state);
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