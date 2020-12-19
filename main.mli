type state
type key
type game

val window_init: string -> unit
val select_ghosts: int ->int
val init_game: string -> int -> int -> Map.fruit array -> int -> int -> game
val check_key: char -> bool
val check_space: game -> key -> char -> game
val update_active_game: game -> key -> char -> State.t -> game
val check_fruits: game -> State.t -> unit
val update_active: game -> key -> game
val update_paused: game -> key -> game
val update_win: game -> key -> game
val update_lose: game -> game
val update_loading: game -> game
val update_waiting: game -> game
val draw_labels: game -> unit
val draw_fruits: game -> unit
val draw_end_game: game -> unit
val draw: game -> unit
val update: game -> unit
val main: string -> unit