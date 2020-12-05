open Graphics 
open Images

type t = {
  sprite : Images.t; 
  height : int;
  width : int;
}

let load_png str = Png.load_as_rgb24 ("./sprites/" ^ str) []

let make_sprite str = {
  sprite = load_png str; 
  height = 45; 
  width = 45;
}

let sprite_from_sheet (sheet: Images.t) (x: int) (y: int) (width: int) 
    (height: int) (shift: int) : t = 
  let sheet_size = Images.size sheet in 
  let x_dim = fst sheet_size in 
  let x_pos = x * width in
  let y_pos = y * height in 
  let x_pos = if x_pos + width + shift < x_dim then x_pos + shift else x_pos in 
  let img = Images.sub sheet x_pos y_pos width height in 
  {sprite = img; height = height; width = width}

let sprite_h sprite = 
  sprite.height

let sprite_w sprite = 
  sprite.width

let sprite_image sprite = 
  sprite.sprite
