(* Gtk render window of a simulated world *)

open Graphics;;

open Geometry;;
open Simulation;;

let rec render_obstacles surface =
  List.iter (fun line -> print_line red line surface)
;;

let render world box =
   let array = Array.make_matrix box.height box.width black in
   render_obstacles array world.obstacles;
   make_image array;
;;
