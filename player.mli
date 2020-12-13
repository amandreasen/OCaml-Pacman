type direction = Up | Right | Down | Left

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

(** [player_direction] is the direction that the player is facing. The 
    direction is a variant of Up/Right/Down/Left. *)
val player_direction: t -> direction

(** [player_prev_move] is the previous move that the player made. *)
val player_prev_move: t -> int * int 

(** [player_prev_attempt] is the last attempted move made by the player. *)
val player_prev_attempt: t -> int * int

(** [move_attempt] updates the player's last move attempted. *)
val move_attempt: t -> int * int -> unit