(** 

   user's points
   lives

   list of ghosts

   map

   food powerups - duration

   current level

   object sensing - walls/food/ghosts

*)
open Ghost
open Map
type t = 
  {
    points:int;
    lives:int;
    ghosts: Ghost.t list;
    current_level: int;
    map: Map.t;
  }

let points state =
  state.points

let lives state =
  state.lives

let ghosts state =
  state.ghosts

let current_level state =
  state.current_level

let update_state state tile=
  let new_points = state.points + tile.food in
  let new_map = state.map in
  let new_level=state.level in
  let new_lives = state.lives in
  let new_ghosts = state.ghosts in
  let new_state = {points:new_points; lives: new_lives; ghosts: new_ghosts; current_level:new_level; map:new_map} in
  new_state

