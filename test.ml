open OUnit2
open Player
open Ghost
open Map

let pp_tuple coordinates = 
  "( " ^ string_of_int (fst coordinates) ^ " , " ^ 
  string_of_int (snd coordinates) ^ " )"

let player_move_pos_test  
    (name : string) 
    (player : Player.t)
    (input_dir : int * int)
    (expected_output : int * int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Player.move player input_dir; 
                                    Player.get_position player)
        ~printer:pp_tuple)

let player_1 = new_player

let ghost_1 = new_ghost 225 275 (50,0) [] 

let player_tests =
  [
    player_move_pos_test "Initial position at (175,175)"
      new_player (0,0) (175,175);
    player_move_pos_test "Initial player moves up 50" 
      player_1 (0,50) (175,225);
    player_move_pos_test "After moving up 50, move right 50" 
      player_1 (50,0) (225,225);
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

let ghost_2 = new_ghost 0 0 (0,0) []

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

let suite =
  "test suite"  >::: List.flatten [
    player_tests;
    ghost_tests;
  ]

let _ = run_test_tt_main suite
