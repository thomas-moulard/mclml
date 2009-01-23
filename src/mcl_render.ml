(* Monte Carlo localization algorithm rendering *)

open Graphics;;

open Geometry;;
open Mcl;;
open Simulation;;

let rec render_mcl surface = function
  | [] -> ()
  | (score, position)::positions ->
      draw_position surface yellow position 5;
      render_mcl surface positions
;;
