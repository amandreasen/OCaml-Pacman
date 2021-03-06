(** Testing Plan: Because the majority of the game can be tested visually, the 
    majority of our testing was done by play testing. By setting our initial 
    number of lives to a higher value than normal, we were able to get to higher
    levels and ensure our functionality works as expected. However, we were able 
    to test functions that are not involved in drawing. In our unit test cases, 
    we used a combination of black-box and glass-box testing to verify our code.
    The majority of these test cases are focused on verifying the correctness of 
    functions in [player.mli], [ghost.mli], and [map.mli]; some of these test 
    cases involve a series of function applications which allows a clear 
    analysis of correctness in the system. The functions in [constants.mli], 
    [main.mli], [state.mli], and [sprite.mli] could not be tested in an 
    automated way and were tested through play testing. We used bisect coverage 
    to ensure that our tests cover as much functionality as possible. Our 
    testing demonstrates the correctness of the system because it has a 
    quantitative measurement for the code tested with unit cases, and the visual 
    aspects are clearly either correct or incorrect when we launch the game. *)

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

let player_position_test
    (name : string)
    (player : Player.t)
    (expected_output : int * int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Player.get_position player)
        ~printer:pp_tuple)

let player_move_pos_test 
    (name : string)
    (player : Player.t)
    (input_dir : int * int)
    (expected_output : int * int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Player.move player input_dir;
                                    Player.get_position player)
        ~printer:pp_tuple)

let player_prev_move_test
    (name : string)
    (player : Player.t)
    (input_dir : int * int): test =
  name >:: (fun _ ->
      assert_equal input_dir (Player.move player input_dir;
                              Player.player_prev_move player) 
        ~printer:pp_tuple)

let player_move_attempt_test
    (name : string)
    (player : Player.t)
    (input_dir : int * int): test =
  name >:: (fun _ ->
      assert_equal input_dir (Player.move_attempt player input_dir;
                              Player.player_prev_attempt player)
        ~printer:pp_tuple)

let player_death_ended_test 
    (name: string)
    (player: Player.t)
    (expected_output: bool) : test = 
  name >:: (fun _ -> assert_equal expected_output (Player.death_ended player)
               ~printer: string_of_bool)

let player_1 = new_player()

let player_tests =
  [
    player_position_test "Initial position" (new_player()) (175,175);
    player_move_pos_test "Initial position at (175,175)"
      (new_player ()) (0,0) (175,175);
    player_move_pos_test "Initial player moves up 50"
      player_1 (0,50) (175,225);
    player_position_test "New position after moving 50" player_1 (175,225);
    player_move_pos_test "After moving up 50, move right 50"
      player_1 (50,0) (225,225);
    player_move_pos_test "moving (0,0) doesn't move the player" player_1 
      (0,0) (225,225);

    player_prev_move_test "starting at (225,275), move up one tile (0,50)"
      player_1 (0,50);
    player_prev_move_test "starting at (225,275), move up and right (50,50)"
      player_1 (50,50);

    player_move_attempt_test "starting at (225,275), attempt to move down one
   tile (0,-50)" player_1 (0,-50);
    player_move_attempt_test "starting at (225,275), attempt to move left
   (-50,0)" player_1 (-50,0);

    player_death_ended_test "player that has started death animation has 
   not ended it" (player_1 |> start_death) false;
  ]

let ghost_position_test 
    (name : string)
    (ghost : Ghost.t)
    (expected_output : int * int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Ghost.get_position ghost)
        ~printer:pp_tuple)

let ghost_prev_move_test
    (name : string)
    (ghost : Ghost.t)
    (input_dir : int * int): test =
  name >:: (fun _ ->
      assert_equal input_dir (Ghost.move ghost input_dir;
                              Ghost.prev_move ghost) ~printer:pp_tuple)

let start_following_test
    (name : string)
    (ghost : Ghost.t)
    (expected_output : bool): test =
  name >:: (fun _ ->
      assert_equal expected_output (Ghost.start_following ghost;
                                    Ghost.is_following ghost)
        ~printer: string_of_bool)

let stop_following_test
    (name : string)
    (ghost : Ghost.t)
    (expected_output : bool): test =
  name >:: (fun _ ->
      assert_equal expected_output (Ghost.reset_following ghost;
                                    Ghost.is_following ghost)
        ~printer: string_of_bool)

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

let done_initializing_test
    (name : string)
    (ghost : Ghost.t)
    (expected_output : bool): test =
  name >:: (fun _ ->
      assert_equal expected_output (Ghost.is_done_initializing ghost)
        ~printer: string_of_bool)

let move_init_counter_test 
    (name : string)
    (ghost : Ghost.t)
    (expected_output : int): test =
  name >:: (fun _ ->
      assert_equal expected_output (Ghost.init_counter ghost)
        ~printer: string_of_int)

let ghost_state_test 
    (name : string)
    (ghost : Ghost.t)
    (state : string): test =
  name >:: (fun _ ->
      assert_equal state (Ghost.set_state ghost state; Ghost.get_state ghost)
        ~printer: (fun x -> x))

let ghost_cyan = new_ghost 225 275 (50,0) "cyan"
let ghost_red = new_ghost 0 0 (0,0) "red"
let ghost_pink = new_ghost 675 125 (0,0) "pink"
let ghost_orange = new_ghost 175 175 (0,50) "orange"

let ghost_tests =
  [
    ghost_position_test "initial position" ghost_red (0,0);
    ghost_position_test "start position at (225,275) and move (0,0)"
      (Ghost.move ghost_cyan (0,0); ghost_cyan) (225,275);
    ghost_position_test "start position at (675,125) and move (100,-50)"
      (Ghost.move ghost_pink (100,-50); ghost_pink) (775,75);
    ghost_position_test "moving orange ghost doesn't move cyan ghost"
      (Ghost.move ghost_orange (50,0); ghost_cyan) (225,275);

    ghost_prev_move_test "starting at (225,275), move up one tile (0,50)"
      ghost_cyan (0,50);
    ghost_prev_move_test "starting at (225,275), move up and right (50,50)"
      ghost_cyan (50,50);
    ghost_prev_move_test "starting at (175,175), move up one tile (0,50)"
      ghost_orange (0,50);

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
    following_counter_test 
      "incremented following of a new ghost is a following count of 1" 
      (Ghost.incr_following_count ghost_orange; ghost_orange) 1;

    move_made_test "new ghost has made a move" ghost_orange true;
    move_made_test "when reset, the ghost has not made a move"
      (Ghost.reset_move ghost_red; ghost_red) false;
    move_made_test "once reset, then the ghost makes a move" 
      (Ghost.reset_move ghost_cyan; Ghost.move ghost_cyan (0,0); ghost_cyan)
      true;

    done_initializing_test "a new ghost is not done initializing" ghost_red
      false;
    done_initializing_test 
      "after finishing initializing, ghost is done initializing" 
      (Ghost.finish_initializing ghost_pink; ghost_pink) true;

    move_init_counter_test "new ghost has 0 init counter" ghost_pink 0;
    move_init_counter_test "after one move, ghost has 1 init counter"
      (Ghost.move_init ghost_cyan (0,0); ghost_cyan) 1;
    move_init_counter_test 
      "after initial move made by orange, red still has 0 init counter" 
      (Ghost.move_init ghost_orange (0,0); ghost_red) 0;

    ghost_state_test "active state" ghost_cyan "active";
    ghost_state_test "scared1 state" ghost_orange "scared1";
    ghost_state_test "scared2 state" ghost_pink "scared2";
    ghost_state_test "eaten state" ghost_red "eaten";
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

let check_move_test
    (name : string)
    (pos : point)
    (map : Map.t)
    (dir: point)
    (initialized: bool)
    (expected_output : bool): test =
  name >:: (fun _ ->
      assert_equal expected_output (Map.check_move pos map dir initialized)
        ~printer: string_of_bool)

let map_init_moves 
    (name : string)
    (map : Map.t)
    (expected_output : Constants.point array array): test = 
  name >:: (fun _ ->
      assert_equal expected_output (Map.initial_ghost_moves map))

let map_init_positions 
    (name : string)
    (map : Map.t)
    (expected_output : Constants.point array): test = 
  name >:: (fun _ ->
      assert_equal expected_output (Map.ghost_init_positions map))

let get_corner_test 
    (name: string)
    (map: Map.t)
    (pos: point)
    (expected_output: Constants.point) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Map.get_corner map pos) ~printer: pp_tuple)

let cherry =
  let img = sprite_from_sheet sprite_sheet 2 3 fruit_width fruit_height 2 in
  {sprite = img; points = 100}

let standard_map = make_map (100, 100) "standard" cherry

let standard_ghost_moves = 
  [|[|(0,0); (move_amt, 0); (0, move_amt); (0,move_amt)|]; 
    [|(0,move_amt); (0,move_amt); (-move_amt, 0)|];
    [|(0,move_amt); (0,move_amt); (move_amt, 0)|];
    [|(0,0); (-move_amt, 0); (0, move_amt); (0,move_amt)|];|]

let standard_ghost_positions = 
  [|(675, 375); (725,375); (775, 375); (825,375);|]

let ocaml_map = make_map (0, 0) "OCaml" cherry

let ocaml_ghost_moves = 
  [|[|(0,-move_amt); (0,-move_amt)|]; [|(0,-move_amt); (0,-move_amt)|];
    [|(0,0); (0,0); (0,-move_amt); (0,-move_amt)|];
    [|(0,0); (0,0); (0,-move_amt); (0,-move_amt)|]|]

let ocaml_ghost_positions = 
  [|(675, 325); (725,325); (675, 375); (725,375)|]

let cs3110_map = make_map (0, 0) "3110" cherry

let cs31110_ghost_moves = 
  [|[|(0,-move_amt); (0,-move_amt); (-move_amt, 0)|]; 
    [|(0,-move_amt); (0,-move_amt)|]; [|(0,-move_amt); (0,-move_amt)|];
    [|(0,-move_amt); (0,-move_amt); (move_amt, 0)|]|]

let cs3110_ghost_positions = 
  [|(575, 375); (625, 375); (675, 375); (725, 375)|]

let map_tests = [
  food_count_test "count initial food in standard map" standard_map 134;
  food_count_test "count initial food in OCaml map" ocaml_map 130;
  food_count_test "count initial food in CS3110 map" cs3110_map 133;

  tile_value_test "tile value of food tile - OCaml map" (175, 175) ocaml_map 1;
  tile_value_test "tile value of empty tile - OCaml map" (425, 375) ocaml_map 0;
  tile_value_test "tile value of ghost tile - OCaml map" (675, 325) ocaml_map 0;
  tile_value_test "tile value of wall tile - OCaml map" (125, 125) ocaml_map 0;
  tile_value_test "tile value of wall tile - standard map" (125, 125)
    standard_map 0;
  tile_value_test "tile value of wall tile - cs3110 map" (125, 125)
    cs3110_map 0;

  get_tile_type_test "food in ocaml map" (225, 175) ocaml_map "Food";
  get_tile_type_test "wall in ocaml map" (425, 375) ocaml_map "Wall";
  get_tile_type_test "ghost in ocaml map" (675, 325) ocaml_map "Ghost";
  get_tile_type_test "wall in standard map" (600, 300) standard_map "Wall";
  get_tile_type_test "food in cs 3110 map" (600, 250) cs3110_map "Food";

  check_move_test "move in wall in ocaml map test 1" (225,175) ocaml_map (0,10) 
    true false;
  check_move_test "move in wall in ocaml map test 2" (225,175) ocaml_map (0,0) 
    true true;
  check_move_test "move in wall in cs3110 map test 1" (125,180) cs3110_map 
    (10,0) true false;
  check_move_test "move in wall in cs3110 map test 2" (600,255) cs3110_map 
    (0,10) true false;
  check_move_test "move in wall in standard map" (225,175) standard_map (0,0) 
    true  true;
  check_move_test "move in wall in standard map" (235,190) standard_map (10,0)
    true false;

  map_init_moves "initial moves for standard map" standard_map 
    standard_ghost_moves;
  map_init_moves "initial moves for ocaml map" ocaml_map ocaml_ghost_moves;
  map_init_moves "initial moves for cs3110 map" cs3110_map cs31110_ghost_moves;


  map_init_positions "initial positions for standard map" standard_map 
    standard_ghost_positions;
  map_init_positions "initial positions for ocaml map" ocaml_map 
    ocaml_ghost_positions;
  map_init_positions "initial positions for cs3110 map" cs3110_map 
    cs3110_ghost_positions;

  get_corner_test "corner of tile center" standard_map (175, 175) (150, 150);
  get_corner_test "corner of tile corner" standard_map (100, 100) (100, 100);
  get_corner_test "corner of non-center tile position" standard_map (203, 167)
    (200, 150)
]

let suite =
  "test suite"  >::: List.flatten [
    player_tests;
    ghost_tests;
    map_tests;
  ]

let _ = run_test_tt_main suite