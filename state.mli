open Ghost
open Player
open Map 
open Sprite

(**[State.t] represents a game level. It contains information about the current
   map, the player, and the ghosts in the level.*) 
type t 

(**[init_level map fruit num_ghosts lives] will initialize and return a level 
   using the map with name [map]. It will randomly spawn the fruit [fruit] when
   enough food has been eaten. Additionally, the map will initialize the number 
   of ghosts to be [num_ghosts] and the number of player lives to be [lives]. 
   All other values are standard across levels and are initialized 
   appropriately.*)
val init_level: string -> fruit -> int -> int -> t

(**[update_level state key] will return a State.t representing the state [state]
   after it has been updated with the key press [key] interpreted as user input
*) 
val update_level: t -> char -> t

(** [draw_game game player_visible] will draw the level [game] to the 
    current GUI window. The player will be drawn if [player_visible] is true 
    and will not be drawn otherwise. *) 
val draw_game: t -> bool -> unit

(**[check_win state] will return 1 if the player has won the level (all food
   has been eaten and player has at least one life left), -1 if the player
   has lost (player does not have any lives left), or 0 if the player 
   has neither won or lost the current level. *)
val check_win: t -> int

(**[points state] will return the number of points in the current level. *)
val points: t -> int

(**[lives state] will return the number of lives left in the current level. *)
val lives: t -> int

(**[fruit_eaten state] will return a boolean value that is true if the fruit 
   in the current level has been eaten and false otherwise. *)
val fruit_eaten: t -> bool

(**[check_visibility state] will return a boolean of whether the player 
   should be visible or not in the current frame of the level [state].*) 
val check_visibility: t -> bool 

(** [animate_win level] will play a win animation for the current level [level]
    on the current open GUI window. *) 
val animate_win: t -> unit
