open Ghost
open Player
open Map 
open Sprite

type t 

(**[init_level map] will initialize a level using the map with name [map]
   by returning a State.t with the appropriate initial values.*) 
val init_level: string -> t

(**[update_level state] will return a State.t representing the state [state] 
   after it has been updated. *) 
val update_level: t -> char -> t

(* val draw_game: t -> unit *)

(* * [player] is the current player in the game.
   val player: t -> Player.t

   (** [get_points] is the current player's points. *)
   val points: t -> int

   (** [lives] is the number of lives the currrent player has left*)
   val lives: t -> int

   (** [ghosts] is the array of ghosts in the current game *)
   val ghosts: t -> Ghost.t array

   (** [current_level] is the current level of the player *)
   val current_level: t -> int

   (** [map] is the current map being played on. *)
   val map: t -> Map.t

   (** [initial_state] initialies a new state for a new player *)
   val initial_state: Player.t -> Map.t -> Ghost.t array -> t

   (**[update_state state map] will return the new state after food has been 
   consumed and the life counter has been updated, if applicable. *) 
   val update_state: t -> Map.t -> t

   (** [update_state_food] is the state after food has been consumed. *)
   val update_state_food: t -> int -> t

   val update_state_lives: t -> t

   (** [make_ghosts] is the array of n number of ghosts where the second and third 
    inputs are the minimum x and y values, respectively. Initially the ghosts 
    are all lined up in a row horizontally starting from the minimum x and y 
    values. *)
   val make_ghosts: int -> int -> int -> Ghost.t array  *)


(* val lives_img_lst: t -> Sprite.t list *)