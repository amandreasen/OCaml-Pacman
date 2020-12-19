(** [state] is the state of the game. *)
type state

(** [key] is char representing the direction of movement. *)
type key

(** [game] represents the current game. *)
type game

(** [window_init] creates the initial window. *)
val window_init: string -> unit

(** [select_ghosts] sets the number of ghosts based on the current level. *)
val select_ghosts: int ->int

(** [init_game] sets the features for starting a new game. *)
val init_game: string -> int -> int -> Map.fruit array -> int -> int -> game

(** [check_key] ensures that the key is valid. *)
val check_key: char -> bool

(** [check_space] checks the status of the game. *)
val check_space: game -> key -> char -> game

(** [update_active_game] updates type game with the winning status. *)
val update_active_game: game -> key -> char -> State.t -> game

(** [check_fruits] checks if the fruit is eaten and updates the fruit 
    in the game. *)
val check_fruits: game -> State.t -> unit

(** [update_active] sets the next key, checks the fruit and updates the game. *)
val update_active: game -> key -> game

(** [update_paused] updates the game if it is no longer paused, 
    or keeps it paused. *)
val update_paused: game -> key -> game

(**[update_win] updates the game window if the player passed a level. *)
val update_win: game -> key -> game

(** [update_lose] updates the game window if the player loses. *)
val update_lose: game -> game

(** [update_loading] updates the game to the next level. *)
val update_loading: game -> game

(** [update_waiting] sets the game ready to play. *)
val update_waiting: game -> game

(** [draw_labels] draws the points and level of the game. *)
val draw_labels: game -> unit

(** [draw_fruits] draws the fruits in the game. *)
val draw_fruits: game -> unit

(** [draw_end_game] draws the end game screen if the player lost. *)
val draw_end_game: game -> unit

(** [draw] draws the whole game screen *)
val draw: game -> unit

(** [update] manages updating the game with each new key. *)
val update: game -> unit

(** [main] starts the game, including setting the initial window and map. *)
val main: string -> unit