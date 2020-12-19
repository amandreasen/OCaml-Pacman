open OUnit2
open Ghost
open Map
open Player
open Sprite
open Constants

let pp_tuple coordinates = 
  let x = fst coordinates in 
  let x_str = string_of_int x in 
  let y = snd coordinates in 
  let y_str = string_of_int y in 
  "( " ^ x_str ^ " , " ^ y_str ^ " )"

let player_move_pos_test  
    (name : string) 
    (player : Player.t)
    (input_dir : int * int)
    (expected_output : int * int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Player.move player input_dir; 
                                    Player.get_position player)
        ~printer:pp_tuple)

(* let player_direction_test 
    (name : string) 
    (player : Player.t)
    (input_dir : int * int)
    (expected_output : Player.direction) : test = 
   name >:: (fun _ -> 
      assert_equal expected_output (Player.move player input_dir; 
                                    Player.player_direction player)) *)

let player_prev_move_test
    (name : string) 
    (player : Player.t)
    (input_dir : int * int): test = 
  name >:: (fun _ -> 
      assert_equal input_dir (Player.move player input_dir; 
                              Player.player_prev_move player)) 

let player_1 = new_player()

let player_tests =
  [
    player_move_pos_test "Initial position at (175,175)"
      (new_player ()) (0,0) (175,175);
    player_move_pos_test "Initial player moves up 50" 
      player_1 (0,50) (175,225);
    player_move_pos_test "After moving up 50, move right 50" 
      player_1 (50,0) (225,225);

    (* player_direction_test "direction after moving (50,0) is Right" 
       player_1 (50,0) Right; 
       player_direction_test "direction after moving (-50,0) is Left" 
       player_1 (-50,0) Left; 
       player_direction_test "direction after moving (0,50) is Up" 
       player_1 (0,50) Up; 
       player_direction_test "direction after moving (0,-50) is Down" 
       player_1 (0,-50) Down;  *)

    player_prev_move_test "starting at (225,275), move up one tile (0,50)"
      player_1 (0,50);
    player_prev_move_test "starting at (225,275), move up and right (50,50)"
      player_1 (50,50);
  ]

let ghost_move_pos_test  
    (name : string) 
    (ghost : Ghost.t)
    (input_dir : int * int)
    (expected_output : int * int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Ghost.move ghost input_dir; 
                                    Ghost.get_position ghost) 
        ~printer:pp_tuple)

let ghost_prev_move_test 
    (name : string) 
    (ghost : Ghost.t)
    (input_dir : int * int): test = 
  name >:: (fun _ -> 
      assert_equal input_dir (Ghost.move ghost input_dir; 
                              Ghost.prev_move ghost)) 

let start_following_test 
    (name : string) 
    (ghost : Ghost.t)
    (expected_output : bool): test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Ghost.start_following ghost; 
                                    Ghost.is_following ghost)) 

let stop_following_test 
    (name : string) 
    (ghost : Ghost.t)
    (expected_output : bool): test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Ghost.reset_following ghost; 
                                    Ghost.is_following ghost)) 

let following_counter_test 
    (name : string) 
    (ghost : Ghost.t)
    (expected_output : int): test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Ghost.following_counter ghost) 
        ~printer: string_of_int)

let move_made_test 
    (name : string) 
    (ghost : Ghost.t)
    (expected_output : bool): test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Ghost.made_move ghost)
        ~printer: string_of_bool)

let ghost_cyan = new_ghost 225 275 (50,0) "cyan" 
let ghost_red = new_ghost 0 0 (0,0) "red"
let ghost_pink = new_ghost 50 50 (0,0) "pink"
let ghost_orange = new_ghost 175 175 (0,50) "orange" 

let ghost_tests = 
  [
    ghost_move_pos_test "start position at (225,275) and move (0,0)" 
      ghost_cyan (0,0) (225,275);
    ghost_move_pos_test "start position at (225,275) and move (100,-50)" 
      ghost_cyan (100,-50) (325,225);

    ghost_prev_move_test "starting at (225,275), move up one tile (0,50)"
      ghost_cyan (0,50);
    ghost_prev_move_test "starting at (225,275), move up and right (50,50)"
      ghost_cyan (50,50);

    start_following_test "new ghost at position (0,0) starts following" 
      ghost_red true;
    start_following_test "ghost that is already following starts following" 
      ghost_red true;

    stop_following_test "ghost that is following now stops following"
      ghost_red false;
    stop_following_test "ghost that is not following stops following"
      ghost_red false;

    following_counter_test "new ghost has following count of 0 "
      ghost_pink 0;
    following_counter_test "incremented following of a new ghost is a following 
    count of 1" (Ghost.incr_following_count ghost_orange; ghost_orange) 1;

    move_made_test "new ghost has made a move" ghost_orange true;
    move_made_test "when reset, the ghost has not made a move" 
      (Ghost.reset_move ghost_red; ghost_red) false;
    move_made_test "once reset, then the ghost makes a move"  
      (Ghost.reset_move ghost_cyan; Ghost.move ghost_cyan (0,0); ghost_cyan) 
      true;
  ]

let food_count_test 
    (name: string)
    (map: Map.t)
    (expected_output: int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (food_count map) ~printer: string_of_int)

let tile_value_test 
    (name: string)
    (point: point)
    (map: Map.t)
    (expected_output: int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Map.get_tile_value point map) 
        ~printer: string_of_int)

let get_tile_type_test
    (name : string)
    (pos : point)
    (map : Map.t)
    (expected_output : string): test =
  name >:: (fun _ ->
      assert_equal expected_output (Map.get_tile_type pos map) 
        ~printer: (fun x -> x))

let cherry = 
  let img = sprite_from_sheet sprite_sheet 2 3 fruit_width fruit_height 2 in 
  {sprite = img; points = 100}

let standard_map = make_map (0, 0) "standard" cherry 
let ocaml_map = make_map (0, 0) "OCaml" cherry
let cs3110_map = make_map (0, 0) "3110" cherry

let map_tests = [
  food_count_test "count initial food in standard map" standard_map 134;
  food_count_test "count initial food in OCaml map" ocaml_map 130;
  food_count_test "count initial food in CS3110 map" cs3110_map 133;
  tile_value_test "tile value of food tile - OCaml map" (175, 175) ocaml_map 1;
  tile_value_test "tile value of empty tile - OCaml map" (425, 375) ocaml_map 0;
  tile_value_test "tile value of ghost tile - OCaml map" (675, 325) ocaml_map 0;
  tile_value_test "tile value of wall tile - OCaml map" (125, 125) ocaml_map 0;
  get_tile_type_test "food in ocaml map" (225, 175) ocaml_map "Food";
  get_tile_type_test "wall in ocaml map" (425, 375) ocaml_map "Wall";
  get_tile_type_test "ghost in ocaml map" (675, 325) ocaml_map "Ghost";
  get_tile_type_test "wall in standard map" (600, 300) standard_map "Wall";
  get_tile_type_test "food in cs 3110 map" (600, 300) cs3110_map "Food";
]

(* 
let check_win_test 
    (name: string)
    (state: State.t)
    (expected_output: int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (State.check_win state))

let init_state = State.init_level "OCaml" cherry

let state_tests = [
  check_win_test "neither win nor lose in initial level state" init_state 0;
] *)

let suite =
  "test suite"  >::: List.flatten [
    player_tests;
    ghost_tests;
    map_tests;
  ]

let _ = run_test_tt_main suite
