(** [state] is the state of the game. *)
type state

(** [key] is char representing the direction of movement. *)
type key

(** [game] represents the current game. *)
type game

(** [window_init settings] creates the initial window with the given 
    [settings]. *)
val window_init: string -> unit

(** [select_ghosts level] sets the number of ghosts based on the current 
    [level]. *)
val select_ghosts: int ->int

(** [init_game map_name points level fruit_basket next_fruit lives] sets the 
    features for starting a new game. It sets the correct [level], the map given
    by [map_name], the [points] the current user has, the fruits available in 
    the game [fruit_basket], the next fruit to be displayed [next_fruit], and 
    the number of lives left for the ghost [lives] *)
val init_game: string -> int -> int -> Map.fruit array -> int -> int -> game

(** [draw game] draws the whole game screen, including the fruits, player,
    and the game. *)
val draw: game -> unit

(** [main settings] starts the game, including setting the initial window and 
    map with the given [settings]. *)
val main: string -> unit