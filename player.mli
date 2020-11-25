type t

(** [new_player] is a hard-coded player with initial coordinates centered at 
    (175,175). *)
val new_player: t

(** [get_position] returns the Player t's current position as a tuple (x, y).*)
val get_position: t -> int * int 

(** [move] updates Player t's current position based on the direction of the 
    user's input. The direction is passed in as a tuple that represents the 
    change in coordinates: ex. (0,1) means the user moves +1 in the y 
    direction.*)
val move: t -> int * int -> unit 


val player_image: t -> Sprite.t