(*This file contains a list of constants used in the game *)

type point = int * int
(*Game Constants*)

(** [window_width] is the width of the window in the game. *)
val window_width: int

(** [window_height] is the height of the window in the game. *)
val window_height: int

(** [map_width] is the width of the map in the game. *)
val map_width: int

(** [map_height] is the height of the map in the game. *)
val map_height: int

(** [ghost_radius] is the radius of the ghost in the game. *)
val ghost_radius: int

(** [player_radius] is the radius of the player in the game. *)
val player_radius: int

(** [player_width] is the width of the player in the game. *)
val player_width: int

(** [player_height] is the height of the player in the game. *)
val player_height: int

(** [init_pos] is the initial position of the player in the game. *)
val init_pos: point

(** [move_amt] is the amount the player moves during each step in the game. *)
val move_amt: int

(** [sleep_time] is the sleep time while the game resets after a death or a new
    level. *)
val sleep_time: float

(** [closeness_threshold] encompassess the nearest 8 tiles around the user.  *)
val closeness_threshold: float

(** [max_follow_time] is the maximum number of seconds that a ghost will follow
    the user. *)
val max_follow_time: float

(** [max_role_rev_time] is the maximum number of seconds that a player will 
    follow the ghost. *)
val max_role_rev_time: int

(** [sprite_sheet] is the list of images used throughout the game. *)
val sprite_sheet: Images.t

(* Food Constants *)

(** [food_val] is the points for each food. *) 
val food_val: int

(** [special_val] is the points for each special food. *)
val special_val: int

(** [food_radius] is the radius of the food. *)
val food_radius: int

(** [special_radius] is the radius of the special food. *)
val special_radius: int

(** [fruit_width] is the width of each food. *)
val fruit_width: int

(** [fruit_height] is the height of each food. *)
val fruit_height: int

(** [fruit_color] is color of food. *)
val food_color: Graphics.color

(** [special_color] is color of special food. *)
val special_color: Graphics.color

(* Map Constants *)

(** [tile_size] is the size of each tile in the map. *)
val tile_size: int

(** [wall_width] is the width of the tile in the map. *)
val wall_width: int

(** [wall_color] is color of the wall. *)
val wall_color: Graphics.color

(*State constants*)

(** [fruit_limit] is maximum number of fruit in a game. *)
val fruit_limit: int

(** [fruit_timer] is amount of time a fruit is available in a game. *)
val fruit_timer: int

(** [png_wl] is the width of the png image. *)
val png_wl: int

(** [ghost_width] is the pixel width of a ghost image. *) 
val ghost_width: int

(** [ghost_height] is the pixel height of a ghost image. *) 
val ghost_height: int 

(** [eaten_time] is the amount of time a ghost will stay in the eaten state 
    after being eaten by a player while role reversal is active. *) 
val eaten_time: int

(** [eaten_threshold] is the amount of time a ghost will wait before turning 
    white after being eaten by a player. *) 
val eaten_threshold: int

(** [label_time] is the amount of time a label should be displayed.*) 
val label_time: int
