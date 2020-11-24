open OUnit2
open Player
open Ghost
open Main
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
                                    get_position player) ~printer:pp_tuple)

let player_1 = new_player

let player_tests =
  [
    player_move_pos_test "Initial position at (175,175)" player_1 (0,0) (175,175);
    player_move_pos_test "Initial player moves up 50" player_1 (175,175) (175,225);
    player_move_pos_test "After moving up 50, move right 50" player_1 (175,225) 
      (225,225);
  ]



let suite =
  "test suite"  >::: List.flatten [
    player_tests
  ]

let _ = run_test_tt_main suite
