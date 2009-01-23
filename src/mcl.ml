(* Monte Carlo Localization algorithm *)

open Robot;;

(* localize the robot using Monte Carlo Localization algorithm.
   Returns a list of possible position with a score. *)

let localize robot =
  [(100, robot.pos)]
;;
