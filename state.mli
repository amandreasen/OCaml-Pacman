type t 

(** [get_points] is the current player's points. *)
val points: t -> int

(** [lives] is the number of lives the currrent player has left*)
val lives: t -> int

(** [ghosts] is the list of ghosts in the current game *)
val ghosts: t -> 'a list

(** [map] is the map of the game *)
val map: t -> 'a

(** [current_level] is the current level of the player *)
val current_level: t -> int

(** [undate_map] takes in the state and the tile the player
    is on, and returns a new state with an updated map field *)
val update_map: t -> Map.t -> t
