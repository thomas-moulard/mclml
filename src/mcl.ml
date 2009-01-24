(* Monte Carlo Localization algorithm *)

open Error_model;;
open Geometry;;
open Robot;;

let initialize_particles n position =
  let score = 1. /. (float_of_int n) in
  Array.make n (score, std_error_model position)
;;

let predict positions motion_model =
  Array.iteri
    (fun i (score, position) ->
      positions.(i) <- (score, motion_model position))
    positions
;;

let update robot positions =
  ()
;;

(* localize the robot using Monte Carlo Localization algorithm.
   Returns a list of possible position with a score. *)
let localize robot positions motion_model =
  predict positions motion_model;
  update robot positions
;;
