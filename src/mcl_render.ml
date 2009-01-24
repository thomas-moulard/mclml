(* Monte Carlo localization algorithm rendering *)

open Graphics;;

open Geometry;;
open Mcl;;
open Simulation;;

let render_mcl surface positions =
  let minimum =
    Array.fold_left (fun x (s, p) -> min s x) infinity positions
  and maximum =
    Array.fold_left (fun x (s, p) -> min s x) neg_infinity positions in
  let delta = maximum -. minimum in
  let get_color s =
    let value = s *. 200. /. delta in
    rgb (55 + int_of_float value) 0 0 in

  Array.iter
    (fun (s, p) -> draw_position surface (get_color s) p 5)
    positions
;;
