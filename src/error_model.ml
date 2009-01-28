(* Error model *)

open Geometry;;

let delta_x = 0.75;;
let delta_y = 0.75;;
let delta_t = 0.;;

let delta_dist = 1.;;

let uniform_rand () = Random.float 1.;;

let normal_rand x =
  x *. (sqrt (-2. *. (log (uniform_rand ())))) *. (cos (2. *. pi *. uniform_rand ()))
;;

let round x =
  if x -. floor x >= 0.5 then
    ceil x
  else
    floor x
;;

let std_error_model (x, y, theta) =
  (int_of_float (round ((float_of_int x) +. normal_rand delta_x)),
   int_of_float (round ((float_of_int y) +. normal_rand delta_y)),
   int_of_float (round ((float_of_int theta) +. normal_rand delta_t)))
;;

let dist_std_error_model x =
  int_of_float ((float_of_int x) +. normal_rand delta_dist)
;;
