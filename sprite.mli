type t

type direction 

(** [new_sprite] is the image to be displayed from a file directory. *)
val make_sprite: string -> t

(** [sprite_h] is the height of the sprite. *)
val sprite_h: t -> int

(** [sprite_w] is the width of the sprite. *)
val sprite_w: t -> int

val sprite_image: t -> Images.t

val sprite_direction: t -> direction 