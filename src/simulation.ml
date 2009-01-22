(* 2d world simulation *)

open Geometry;;
open Robot;;

type world = {
    mutable obstacles : line list;
    mutable robots : (robot * int) list;
  }
;;

let make_world obstacles_list robots_list = {
  obstacles = obstacles_list;
  robots = robots_list;
};;

let add_robot world robot =
  world.robots <- robot::world.robots
;;

let make_obstacle world obstacle =
  world.obstacles <- obstacle::world.obstacles
;;

let make_virtual_distance_sensors world position =
  fun () -> 0 (* FIXME: *)
;;

let make_virtual_actuators world position =
  fun n -> () (* FIXME: *)
;;

let update_world world =
  let rec update_robots = function
    | [] -> ()
    | robot::robots ->
        (* FIXME: move robots,
           virtual actuators should set a hook or something
           to be able to move the robot here. *)
        update_robots robots in
  update_robots world.robots
;;
