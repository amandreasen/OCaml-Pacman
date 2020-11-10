open Graphics
open Map
open Player

let window_width = 1500
let window_height = 750
let map_width = 1300
let map_height = 550


let set_window (title: string) (color: Graphics.color) : unit = 
  set_window_title title;
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ())



(* 
let parse_dir (user: Player.t) (dir: string) =
  match dir with 
  |"\033[A" -> (0,50)
  |"\033[B" -> (0,-50)
  |"\033[D" -> (-50,0)
  |"\033[C" -> (50,0)
  |_ -> (0,0)  *)


let parse_dir (user: Player.t) (dir: char) =
  match dir with 
  |'w' ->  (0,50)
  |'s' -> (0,-50)
  |'a' -> (-50,0)
  |'d' -> (50,0)
  |_ -> (0,0)

let rec loop () user = 
  Unix.sleep(0);
  let create_sprite dir = 
    Player.move user dir;
    Graphics.set_color (rgb 255 255 0); 
    Graphics.fill_circle (fst (get_position user)) (snd (get_position user)) 25;
  in
  create_sprite (parse_dir user (Graphics.read_key ())); 
  loop () user

let main (settings: string) : unit = 
  open_graph settings;
  set_window "Pacman" black;
  set_color black;
  fill_rect 0 0 window_width window_height;
  set_color blue;
  draw_rect 100 100 map_width map_height;
  let map = make_map map_width map_height (100,100) in 
  draw_map map;
  Graphics.set_color (rgb 255 255 0); 
  Graphics.fill_circle 175 175 25;
  Graphics.display_mode true;
  ignore (loop () new_player);
  ()


let () = 
  let settings = 
    string_of_int window_width ^ "x" ^ string_of_int window_height
  in
  main (" " ^ settings) 