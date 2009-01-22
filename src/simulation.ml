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


let intersect_position_obstacles world (x, y, theta) =
  let search_obstacles_extremi f =
    let comp_extrema (x_, y_) ((x0,y0),(x1,y1)) =
      (f x_ (f x0 x1), f y_ (f y0 y1)) in
    List.fold_left comp_extrema (max_int, max_int) world.obstacles in
  let (min_x, min_y) = search_obstacles_extremi min
  and (max_x, max_y) = search_obstacles_extremi max in
  let rec intersect = function
    | [] -> max_int
    | obstacle::obstacles ->
        try
          let obstacle_points = points_from_line obstacle in
          let dist_max = max
              (euclidian_distance (x, y) (min_x, min_y))
              (euclidian_distance (x, y) (max_x, max_y)) in
          let (x_, y_) = point_from_position (x, y, theta) dist_max in
          let pos_points = points_from_line ((x_, y_), (x, y)) in
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



let make_virtual_distance_sensor
    world (robot, r, linear_speed, angular_speed) position =
  fun () -> intersect_position_obstacles world position
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
