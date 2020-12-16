type t 

val empty: t

val read_leaderboard: t

val write_leaderboard: t -> unit 

(* val add_new_player: string -> t -> bool *)

val add_player_score: string -> float -> t -> t

val sort_leaderboard: t -> t 
