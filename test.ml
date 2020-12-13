open OUnit2
open Ghost
open Map
open Player

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

let player_direction_test 
    (name : string) 
    (player : Player.t)
    (input_dir : int * int)
    (expected_output : Player.direction) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Player.move player input_dir; 
                                    Player.player_direction player))

let player_prev_move_test
    (name : string) 
    (player : Player.t)
    (input_dir : int * int): test = 
  name >:: (fun _ -> 
      assert_equal input_dir (Player.move player input_dir; 
                              Player.player_prev_move player)) 

let player_1 = new_player

let player_tests =
  [
    player_move_pos_test "Initial position at (175,175)"
      new_player (0,0) (175,175);
    player_move_pos_test "Initial player moves up 50" 
      player_1 (0,50) (175,225);
    player_move_pos_test "After moving up 50, move right 50" 
      player_1 (50,0) (225,225);

    player_direction_test "direction after moving (50,0) is Right" 
      player_1 (50,0) Right; 
    player_direction_test "direction after moving (-50,0) is Left" 
      player_1 (-50,0) Left; 
    player_direction_test "direction after moving (0,50) is Up" 
      player_1 (0,50) Up; 
    player_direction_test "direction after moving (0,-50) is Down" 
      player_1 (0,-50) Down; 

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
      assert_equal expected_output (Ghost.following_counter ghost))

let ghost_1 = new_ghost 225 275 (50,0) "cyan" 
let ghost_2 = new_ghost 0 0 (0,0) "cyan"

let ghost_tests = 
  [
    ghost_move_pos_test "start position at (225,275) and move (0,0)" 
      ghost_1 (0,0) (225,275);
    ghost_move_pos_test "start position at (225,275) and move (100,-50)" 
      ghost_1 (100,-50) (325,225);

    ghost_prev_move_test "starting at (225,275), move up one tile (0,50)"
      ghost_1 (0,50);
    ghost_prev_move_test "starting at (225,275), move up and right (50,50)"
      ghost_1 (50,50);

    start_following_test "new ghost at position (0,0) starts following" 
      ghost_2 true;
    start_following_test "ghost that is already following starts following" 
      ghost_2 true;

    stop_following_test "ghost that is following now stops following"
      ghost_2 false;
    stop_following_test "ghost that is not following stops following"
      ghost_2 false;

    following_counter_test "new ghost has following count of 0 "
      ghost_2 0;
    following_counter_test "incremented following of a new ghost is a following 
    count of 1"
      (Ghost.incr_following_count ghost_2; ghost_2) 0;
  ]

let food_count_test 
    (name: string)
    (map: Map.t)
    (expected_output: int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (food_count map) ~printer: string_of_int)

let standard_map = make_map (0, 0) "standard"
let ocaml_map = make_map (0, 0) "OCaml"

let map_tests = [
  food_count_test "count initial food in standard map" standard_map 134;
  food_count_test "count initial food in OCaml map" ocaml_map 133;
]

let suite =
  "test suite"  >::: List.flatten [
    player_tests;
    ghost_tests;
    map_tests;
  ]

let _ = run_test_tt_main suite
