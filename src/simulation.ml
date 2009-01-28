(* 2d world simulation *)

open Format;;

open Error_model;;
open Geometry;;
open Robot;;

let robot_radius = 10;;

let intersection_hash = Hashtbl.create 100;;

(*
   * REAL position (in the virtual world)
   * position radius
   * linear speed
   * angular speed
*)
type robot_configuration =
    position ref * int * int ref * int ref
;;

type world = {
    mutable obstacles : line list;
    mutable robots_cfg : robot_configuration list;
  }
;;

let make_world obstacles_list robots_cfg_list = {
  obstacles = obstacles_list;
  robots_cfg = robots_cfg_list;
};;

let add_robot world robot =
  let robot_cfg = (ref robot.pos, robot_radius, ref 0, ref 0) in
  world.robots_cfg <- robot_cfg::world.robots_cfg
;;

let make_obstacle world obstacle =
  world.obstacles <- obstacle::world.obstacles;
  Hashtbl.clear intersection_hash
;;


let compute_intersect_position_obstacles world (x, y, theta) =
  let rec intersect = function
    | [] -> max_int
    | obstacle::obstacles ->
        try
          let intersection =
            intersect_position_line (x, y, theta) obstacle in
          min (euclidian_distance intersection (x, y)) (intersect obstacles)
        with No_intersection -> intersect obstacles in
  intersect world.obstacles
;;

let intersect_position_obstacles world position =
  try
    Hashtbl.find intersection_hash position
  with Not_found ->
    let res =
      compute_intersect_position_obstacles world position in
    Hashtbl.add intersection_hash position res;
    res
;;

(* FIXME: test that *)
let precompute_intersections world ((x, y), w, h) =
  for y_ = y to pred (y + h) do
    for x_ = x to pred (x + w) do
      for t = 0 to 359 do
        let _ = intersect_position_obstacles world (x_, y_, t) in
        ()
      done
    done
  done
;;


let make_virtual_distance_sensor
    world (pos, r, linear_speed, angular_speed) position =
  fun () ->
    dist_std_error_model
      (intersect_position_obstacles world (add_position !pos position))
;;

let make_virtual_speed_actuator
    world (pos, r, linear_speed, angular_speed) position =
  fun n -> linear_speed := n
;;

let make_virtual_angle_actuator
    world (pos, r, linear_speed, angular_speed) position =
  fun n -> angular_speed := n
;;


let virtual_robot_move (x, y, theta) linear_speed angular_speed =
  let (x_, y_) =
    point_from_position (x, y, theta) linear_speed in
  let res = std_error_model (x_, y_, wrap_angle (theta + angular_speed)) in
  res
;;

let update_world world =
  let rec update_robots_cfg = function
    | [] -> ()
    | (pos, _, linear_speed, angular_speed)::robots_cfg ->
        pos := virtual_robot_move !pos !linear_speed !angular_speed;
        update_robots_cfg robots_cfg in
  update_robots_cfg world.robots_cfg
;;
