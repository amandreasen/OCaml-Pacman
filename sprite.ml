open Graphics 
open Images

type t = {
  sprite : Images.t; 
  height : int;
  width : int;
}

let load_png str = load ("./sprites/" ^ str) []

let make_sprite str =
  {sprite = load_png str; height = 45; width = 45}

let sprite_h sprite = sprite.height

let sprite_w sprite = sprite.width

let sprite_image sprite = sprite.sprite