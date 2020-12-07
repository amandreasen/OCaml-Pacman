open Ghost
open Player
open Map 
open Sprite

type t 

(** [player] is the current player in the game. *)
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

(** [update_state_food] is the state after food has been consumed. *)
val update_state_food: t -> int -> t

val update_state_lives: t -> Map.t -> t

(** [make_ghosts] is the array of n number of ghosts where the second and third 
    inputs are the minimum x and y values, respectively. Initially the ghosts 
    are all lined up in a row horizontally starting from the minimum x and y 
    values. *)
val make_ghosts: int -> int -> int -> Ghost.t array 


val lives_img_lst: t -> Sprite.t list