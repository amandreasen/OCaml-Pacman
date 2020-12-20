open Graphics 
open Images

type t = Images.t

let sprite_from_sheet (sheet: Images.t) (x: int) (y: int) (width: int) 
    (height: int) (shift: int) : t = 
  let sheet_size = Images.size sheet in 
  let x_dim = fst sheet_size in 
  let x_pos = x * 50 in
  let y_pos = y * 50 in 
  let x_pos = if x_pos + width + shift < x_dim then x_pos + shift else x_pos in 
  Images.sub sheet x_pos y_pos width height 

let sprite_image sprite = 
  sprite
[@@coverage off]