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

let parse_dir (dir: string) =
  match dir with 
  |"\033[A" -> "UP"
  |"\033[B" -> "DOWN"
  |"\033[D" -> "LEFT"
  |"\033[C" -> "RIGHT"
  |_ -> ""

let rec loop () = 
  Unix.sleep(1);
  ignore (parse_dir (read_line ()));
  loop ()

let main (settings: string) : unit = 
  open_graph settings;
  set_window "Pacman" black;
  set_color black;
  fill_rect 0 0 window_width window_height;
  set_color blue;
  draw_rect 100 100 map_width map_height;
  let map = make_map map_width map_height (100,100) in 
  draw_map map;
  draw_player new_player;
  ignore (loop ());
  ()


let () = 
  let settings = 
    string_of_int window_width ^ "x" ^ string_of_int window_height
  in
  main (" " ^ settings) 