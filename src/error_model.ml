(* Error model *)

let std_error_model (x, y, theta) =
  let delta_x = 1
  and delta_y = 1
  and delta_t = 0 in
  let rand delta = Random.int ((2 * delta) + 1) in
  (x - delta_x + rand delta_x,
   y - delta_y + rand delta_y,
   theta - delta_t + rand delta_t)
;;
