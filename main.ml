open Graphics
open Map
open Player
open State

let window_width = 1500
let window_height = 750
let map_width = 1300
let map_height = 550

let move_amt = 12

let game_status state = ("Points: " ^ string_of_int (points state)
                         ^ "   Lives: " ^ string_of_int (lives state)
                         ^ "   Level: " ^ string_of_int (current_level state))

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
  Unix.sleepf(0.2);
  let create_sprite dir = 
    Player.move user dir;
    clear_graph ();
    set_window "Pacman" black;
    set_color blue;
    (**draw_map map;*)
    draw_image map 0 0;
    moveto 175 75;
    set_color white;
    draw_string (game_status state);
    set_color yellow; 
    fill_circle (fst (get_position user)) (snd (get_position user)) 25;
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
  set_color white;
  draw_string (game_status state);
  ignore (loop () new_player map_image state);
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