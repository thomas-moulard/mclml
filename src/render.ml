(* Graphics render window of a simulated world *)

open Graphics;;

open Geometry;;
open Robot;;
open Simulation;;

let render_obstacles surface =
  List.iter (fun line -> draw_line red line surface)
;;

let render_robots surface =
  let draw_sensor pos r (p, _) =
    draw_position surface blue (add_position pos p) r in
  let draw_robot_cfg (pos, radius, _, _) robot =
    draw_position surface green !pos radius;
    List.iter (draw_sensor !pos (radius/2)) robot.dist_sensors;
  in
  List.iter2 draw_robot_cfg
;;

let init_render_image box =
    Array.make_matrix box.height box.width black
;;

let render_world surface world robots =
   render_obstacles surface world.obstacles;
   render_robots surface world.robots_cfg robots;
;;

let draw_render surface =
  draw_image (make_image surface) 0 0;
;;
