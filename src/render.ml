(* Gtk render window of a simulated world *)

open Graphics;;

open Geometry;;
open Robot;;
open Simulation;;

let render_obstacles surface =
  List.iter (fun line -> draw_line red line surface)
;;

let render_robots surface =
  let draw_robot (robot, radius, _, _) =
    draw_position surface green robot.pos radius in
  List.iter draw_robot
;;

let render world box =
   let array = Array.make_matrix box.height box.width black in
   render_obstacles array world.obstacles;
   render_robots array world.robots;
   make_image array;
;;
