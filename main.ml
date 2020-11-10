open Graphics
open Map
(**open Player*)

let window_width = 1500
let window_height = 750
let map_width = 1300
let map_height = 550


let set_window (title: string) (color: Graphics.color) : unit = 
  set_window_title title;
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ())




let parse_dir (dir: string) (plyr: Player.t)=
  let dir_move = 
    match dir with 
    |"\033[A" -> (0,50)
    |"\033[B" -> (0,-50)
    |"\033[D" -> (-50,0)
    |"\033[C" -> (50,0)
    |_ -> (0,0)
  in 
  dir_move
(**move plyr dir_move*)


let rec loop () = (**plyr = *)
  Unix.sleep(1);
  (**ignore (parse_dir (read_line ()) plyr);
     loop () plyr*)
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
<<<<<<< HEAD
  draw_player new_player;
=======
  let create_sprite = 
    Graphics.set_color (rgb 255 255 0);
    Graphics.draw_circle 175 175 25;
    Graphics.display_mode true in
  create_sprite;
>>>>>>> 288fdbaff2c95fb5178989ad690c278339db2a02
  ignore (loop ());
  ()


let () = 
  let settings = 
    string_of_int window_width ^ "x" ^ string_of_int window_height
  in
  main (" " ^ settings) 