open Graphics 
open Images

type direction = Up | Right | Down | Left

type t = {
  sprite : Images.t; 
  height : int;
  width : int;
  direction : direction
}

let load_png str = Png.load_as_rgb24 ("./sprites/" ^ str) []

let make_sprite str = {
  sprite = load_png str; 
  height = 45; 
  width = 45;
  direction = Right
}

let sprite_from_sheet (sheet: Images.t) (coordinate: int * int) (width: int) 
    (height: int) : t = 
  failwith "unimplemented"

let sprite_h sprite = 
  sprite.height

let sprite_w sprite = 
  sprite.width

let sprite_image sprite = 
  sprite.sprite

let sprite_direction sprite = 
  sprite.direction