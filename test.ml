open OUnit2
open Player
open Ghost
open Main
open Map


let pp_tuple coordinates = 
  "( " ^ string_of_int (fst coordinates) ^ " , " ^ 
  string_of_int (snd coordinates) ^ " )"

(** TODO: abstract out the move and position test for plater and ghost *)
(** we may want to abstract out ghost and player in general..  *)

let player_move_pos_test  
    (name : string) 
    (player : Player.t)
    (input_dir : int * int)
    (expected_output : int * int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Player.move player input_dir; 
                                    get_position player) ~printer:pp_tuple)

let ghost_move_pos_test  
    (name : string) 
    (ghost : Ghost.t)
    (input_dir : int * int)
    (expected_output : int * int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Ghost.move ghost input_dir; 
                                    get_pos ghost) ~printer:pp_tuple)

let player_1 = new_player

let ghost_1 = new_g 225 275

let player_tests =
  [
    player_move_pos_test "Initial position at (175,175)"
      player_1 (0,0) (175,175);
    player_move_pos_test "Initial player moves up 50" 
      player_1 (175,175) (175,225);
    player_move_pos_test "After moving up 50, move right 50" 
      player_1 (175,225) (225,225);

    ghost_move_pos_test "start position at (225,275) and move (0,0)" 
      ghost_1 (0,0) (225,275);
    ghost_move_pos_test "start position at (225,275) and move (100,-50)" 
      ghost_1 (100,-50) (325,225);

  ]



let suite =
  "test suite"  >::: List.flatten [
    player_tests
  ]

let _ = run_test_tt_main suite
