open Ghost
open Player
open Map 
open Sprite

type t 

(**[init_level map] will initialize a level using the map with name [map]
   by returning a State.t with the appropriate initial values.*) 
val init_level: string -> fruit -> t

(**[update_level state] will return a State.t representing the state [state] 
   after it has been updated. *) 
val update_level: t -> char -> t

(**[check_win state] will return 1 if the player has won the level (all food
   has been eaten and player has at least one life left), -1 if the player
   has lost (player does not have any lives left), or 0 if the player 
   has neither won or lost the current level.*)
val check_win: t -> int

(**[points state] will return the number of points in the current level. *)
val points: t -> int

(**[lives state] will return the number of lives left in the current level. *)
val lives: t -> int

(**[fruit_eaten state] will return a boolean value that is true if the fruit 
   in the current level has been eaten and false otherwise. *)
val fruit_eaten: t -> bool
