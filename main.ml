open Graphics
open Map
open Player
open State
open Ghost

let window_width = 1500
let window_height = 750
let map_width = 1300
let map_height = 550

let move_amt = 50

let game_status state = 
  ("Points: " ^ string_of_int (points state)
   ^ "   Lives: " ^ string_of_int (lives state)
   ^ "   Level: " ^ string_of_int (current_level state))

let tile_type str = ("Tile type: " ^ str)

let food_count food = ("Food Count: " ^ string_of_int (food))

let check_move b = ("Can Move: " ^ string_of_bool(b))

let set_window (title: string) (color: Graphics.color) : unit = 
  set_window_title title;
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ())

(** [parse_dir] is the tuple representing the change in coordinates from the 
    user's character input. *)
let parse_dir (dir: char) =
  match dir with 
  |'w' ->  (0, move_amt)
  |'s' -> (0, -move_amt)
  |'a' -> (-move_amt, 0)
  |'d' -> (move_amt, 0)
  |_ -> (0,0)

(** [move_ghosts] randomly moves each ghost to a neighboring cell, so long as 
    it is a viable move. *)
let move_ghosts ghosts map = 
  let rec new_g_pos (g : Ghost.t) (dir : int * int ) =  
    Ghost.move g dir 
    (* if Map.check_move (Ghost.get_pos g) map dir 
       then Ghost.move g dir 
       else new_g_pos g rand_dir    *)
  in 
  Array.iter (fun g -> new_g_pos g rand_dir) ghosts 


let flush () = 
  while Graphics.key_pressed () do 
    ignore (Graphics.read_key());
  done;
  ()

let make_move user map dir = 
  if (Map.check_move (get_position user) map dir) 
  then (Player.move user dir;
        check_food (get_position user) map;)

let rec loop () user map state ghosts= 
  Unix.sleepf(0.2);

  make_move user map (parse_dir (Graphics.read_key ()));
  flush ();

  clear_graph ();
  set_window "Pacman" black;
  set_color blue;
  draw_map map;
  (*draw_image map_image 0 0;*)
  moveto 175 75;
  set_color red;
  let new_state = State.update_state_food state map in
  draw_string (game_status new_state);
  (*draw_string (tile_type (Map.get_tile_type (get_position user) map));*)
  (*draw_string (check_move (Map.check_move (get_position user) map dir));*)
  set_color yellow; 
  fill_circle (fst (get_position user)) (snd (get_position user)) 25; 

  move_ghosts ghosts map; (** this line is faulty *)
  set_color cyan;
  for i = 0 to Array.length ghosts - 1 do 
    let g = ghosts.(i) in 
    fill_circle (fst (get_pos g)) (snd (get_pos g)) 25;
  done; 
  loop () user map state ghosts

let main (settings: string) : unit = 
  open_graph settings;
  set_window "Pacman" black;
  set_color black;
  fill_rect 0 0 window_width window_height;
  (* set_color blue;
     draw_rect 100 100 map_width map_height; *)
  let map = make_map (100,100) "OCaml" in 
  draw_map map;
  (* let map_image = get_image 0 0 window_width window_height in  *)
  set_color yellow; 
  fill_circle 175 175 25;
  moveto 175 75;
  let ghost1 = Ghost.new_g 675 375 in 
  set_color cyan;
  fill_circle (fst (get_pos ghost1)) (snd (get_pos ghost1)) 25;
  (* let ghost2 = Ghost.new_g 725 375 in 
     fill_circle (fst (get_pos ghost2)) (snd (get_pos ghost2)) 25; *)
  let ghost_arr = [|ghost1|] in 
  let state = initial_state map ghost_arr in 
  set_color red;
  draw_string (game_status state);
  ignore (loop () new_player map state (ghosts state));
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