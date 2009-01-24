(* Gtk render window of a simulated world *)

open Graphics;;

open Geometry;;
open Robot;;
open Simulation;;

let render_obstacles surface =
  List.iter (fun line -> draw_line red line surface)
;;

let render_robots surface =
  let draw_robot_cfg (pos, radius, _, _) =
    draw_position surface green !pos radius in
  List.iter draw_robot_cfg
;;

let init_render_image box =
    Array.make_matrix box.height box.width black
;;

let render_world surface world =
   render_obstacles surface world.obstacles;
   render_robots surface world.robots_cfg;
;;

let draw_render surface =
  draw_image (make_image surface) 0 0;
;;
