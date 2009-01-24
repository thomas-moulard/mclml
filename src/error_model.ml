(* Error model *)

let delta_x = 1;;
let delta_y = 1;;
let delta_t = 0;;

let delta_dist = 1;;

let rand delta = Random.int ((2 * delta) + 1);;

let std_error_model (x, y, theta) =
  (x - delta_x + rand delta_x,
   y - delta_y + rand delta_y,
   theta - delta_t + rand delta_t)
;;

let dist_std_error_model x =
  x - delta_dist + rand delta_dist
;;
