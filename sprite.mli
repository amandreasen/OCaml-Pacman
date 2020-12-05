open Images

type t

(** [new_sprite] is the image to be displayed from a file directory. *)
val make_sprite: string -> t

(**[sprite_from_sheet sheet x y width height] will make a sprite with a 
   source image of width [width] and height [height] taken from the sprite 
   sheet [sheet] with a top right corner of [(x,y)]. *) 
val sprite_from_sheet: Images.t -> int -> int -> int -> int -> int -> t

(** [sprite_h] is the height of the sprite. *)
val sprite_h: t -> int

(** [sprite_w] is the width of the sprite. *)
val sprite_w: t -> int

val sprite_image: t -> Images.t
