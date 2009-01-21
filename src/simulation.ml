(* 2d world simulation *)

open Geometry;;
open Robot;;

type world = {
    mutable obstacles : line list;
    robots : robot list;
  }
;;

let make_world obstacles_list = {
  obstacles = obstacles_list;
  robots = [];
};;

let make_obstacle world obstacle =
  world.obstacles <- obstacle::world.obstacles
;;
