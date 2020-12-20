(** Player is the Pacman character in the game. The user has control over 
    the Pacman's movements. *)

type t

(** [new_player] is a hard-coded player with initial coordinates centered at 
    (175,175). *)
val new_player: unit -> t

(** [get_position player] is the current position of [player] as a 
    tuple (x, y).*)
val get_position: t -> int * int 

(** [move player dir] updates the current position of [player] based on [dir]. 
    [dir] is a tuple (x,y) that represents the change in coordinates: 
    ex. (0,1) means the user moves +1 in the y direction.
    Requires: [dir] is a possible move that [player] can make in the current 
    map. *)
val move: t -> int * int -> unit 

(** [player_image player] is the Sprite.t representation of a player. *)
val player_image: t -> Sprite.t

(** [player_prev_move player] is the previous move that [player] made. *)
val player_prev_move: t -> int * int 

(** [player_prev_attempt plyaer ] is the last attempted move made by [player].*)
val player_prev_attempt: t -> int * int

(** [move_attempt player dir] updates the previous move attempted by [player] to 
    be [dir]. *)
val move_attempt: t -> int * int -> unit

(**[start_death player] returns a Player.t object that will start its death 
   animation. *) 
val start_death: t -> t 

(**[animate_death player] will update the Player.t [player]'s death animation 
   to the next animation frame, if possible. If the death animation can be 
   updated to the next frame, the function will return true. Otherwise, if the 
   death animation has concluded, the function will return false. *) 
val animate_death: t -> unit 

val death_ended: t -> bool

val reset_move: t -> unit