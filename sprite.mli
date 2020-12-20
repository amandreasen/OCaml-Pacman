open Images

(** Sprite is the image generator for all items displayed in the game. *)

(** Sprite.t is the image conversion of a PNG. *)
type t

(**[sprite_from_sheet sheet x y width height] will make a sprite with a 
   source image of width [width] and height [height] taken from the sprite 
   sheet [sheet] with a top right corner of [(x,y)]. *) 
val sprite_from_sheet: Images.t -> int -> int -> int -> int -> int -> t

(** [sprite_image t] is the image of the Sprite [t]. *)
val sprite_image: t -> Images.t
