(* 2d world simulation *)

open Geometry;;
open Robot;;

type world = {
    mutable obstacles : line list;

    (* robot * radius * linear speed * angular speed *)
    mutable robots : (robot * int * int ref * int ref) list;
  }
;;

let make_world obstacles_list robots_list = {
  obstacles = obstacles_list;
  robots = robots_list;
};;

let add_robot world robot_cfg =
  world.robots <- robot_cfg::world.robots
;;

let make_obstacle world obstacle =
  world.obstacles <- obstacle::world.obstacles
;;

let make_virtual_distance_sensors world robot_cfg position =
  fun () -> 0 (* FIXME: *)
;;

let make_virtual_speed_actuator
    world (robot, r, linear_speed, angular_speed) position =
  fun n -> linear_speed := n
;;

let make_virtual_angle_actuator
    world (robot, r, linear_speed, angular_speed) position =
  fun n -> angular_speed := n
;;


let move position linear_speed angular_speed =
  let (x, y, theta) = position in
  let theta_ = gradient_of_degree (float_of_int theta) in
  let x_ = (cos theta_) *. (float_of_int linear_speed) +. float_of_int x
  and y_ = (sin theta_) *. (float_of_int linear_speed) +. float_of_int y in
  (int_of_float x_, int_of_float y_, theta + angular_speed)
;;

let update_world world =
  let rec update_robots = function
    | [] -> ()
    | robot_data::robots ->
        let (robot, _, linear_speed, angular_speed) = robot_data in
        robot.pos <- move robot.pos !linear_speed !angular_speed;
        update_robots robots in
  update_robots world.robots
;;
