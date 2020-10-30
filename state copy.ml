
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

let map t =
  failwith "Unimplemented"