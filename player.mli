type t

(** [new_player] is a hard-coded player with initial coordinates centered at 
    (175,175). *)
val new_player: unit -> t

(** [get_position] returns the Player t's current position as a tuple (x, y).*)
val get_position: t -> int * int 

(** [move] updates Player t's current position based on the direction of the 
    user's input. The direction is passed in as a tuple that represents the 
    change in coordinates: ex. (0,1) means the user moves +1 in the y 
    direction.*)
val move: t -> int * int -> unit 

(** [player_image] is the Sprite.t representation of a player. *)
val player_image: t -> Sprite.t

(** [player_prev_move] is the previous move that the player made. *)
val player_prev_move: t -> int * int 

(** [player_prev_attempt] is the last attempted move made by the player. *)
val player_prev_attempt: t -> int * int

(** [move_attempt] updates the player's last move attempted. *)
val move_attempt: t -> int * int -> unit

(**[start_death user] returns a Player.t object that will start its death 
   animation. *) 
val start_death: t -> t 

(**[animate_death user] will update the Player.t [user]'s death animation 
   to the next animation frame, if possible. If the death animation can be 
   updated to the next frame, the function will return true. Otherwise, if the 
   death animation has concluded, the function will return false. *) 
val animate_death: t -> unit 

val death_ended: t -> bool

val reset_move: t -> unit