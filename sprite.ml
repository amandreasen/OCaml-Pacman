open Graphics 
open Images

type t = {
  sprite : Images.t;
  height : int;
  width : int;
}

let make_sprite str =
  {sprite = load ("./sprites/" ^ str) []; height = 50; width = 50}
