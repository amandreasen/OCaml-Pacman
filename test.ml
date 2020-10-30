open OUnit2
open Player
(*open Ghost
  open Main
  open Map*)


let pp_tuple coordinates = 
  "( " ^ string_of_int (fst coordinates) ^ " , " ^ 
  string_of_int (snd coordinates) ^ " )"

let player_move_test  
    (name : string) 
    (player : Player.t)
    (input_dir : int * int)
    (expected_output : int * int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (move player input_dir; get_position player)
        ~printer:pp_tuple)

let player_1 = new_player

let player_tests =
  [
    player_move_test "Initial position (0,0)" player_1 (0,0) (0,0);
    player_move_test "Initial player moves up 1" player_1 (0,1) (0,1);
    player_move_test "After moving up 1, move right 1" player_1 (1,0) (1,1);
  ]

let suite =
  "test suite"  >::: List.flatten [
    player_tests
  ]

let _ = run_test_tt_main suite
