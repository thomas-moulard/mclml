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
  let search_obstacles_extremi f def =
    let comp_extrema (x_, y_) ((x0,y0),(x1,y1)) =
      (f x_ (f x0 x1), f y_ (f y0 y1)) in
    List.fold_left comp_extrema def world.obstacles in
  let (min_x, min_y) =
    search_obstacles_extremi min (max_int, max_int)
  and (max_x, max_y) =
    search_obstacles_extremi max (min_int, min_int) in
  let rec intersect = function
    | [] -> max_int
    | obstacle::obstacles ->
        try
          let obstacle_points = points_from_line obstacle in
          let dist_max = max
              (max
                 (euclidian_distance (x, y) (min_x, y))
                 (euclidian_distance (x, y) (x, min_y)))
              (max
                 (euclidian_distance (x, y) (max_x, y))
                 (euclidian_distance (x, y) (x, max_y))) in

          let (x_, y_) = point_from_position (x, y, theta) dist_max in
          let pos_points = List.rev (points_from_line ((x_, y_), (x, y))) in
          let (xo, yo) =
            List.find
              (fun (xo, yo) ->
                List.exists
                  (fun (x, y) -> xo == x && yo == y) pos_points)
              obstacle_points in
          euclidian_distance (x, y) (xo, yo)
        with Not_found -> intersect obstacles in
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
  fun () -> dist_std_error_model (intersect_position_obstacles world !pos)
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
  std_error_model (x_, y_, theta + angular_speed)
;;

let update_world world =
  let rec update_robots_cfg = function
    | [] -> ()
    | (pos, _, linear_speed, angular_speed)::robots_cfg ->
        pos := virtual_robot_move !pos !linear_speed !angular_speed;
        update_robots_cfg robots_cfg in
  update_robots_cfg world.robots_cfg
;;
