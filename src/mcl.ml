(* Monte Carlo Localization algorithm *)

open Format;;

open Error_model;;
open Geometry;;
open Robot;;

exception Empty_particle_array;;
exception Lost;;

let print_mcl_particles particles =
  let print_particle (w, (x, y, theta)) =
    printf "@[<h>(%d, %d, %d): %f@]@\n" x y theta w in
  printf "MCL particles:@\n@[<v 1> ";
  Array.iter print_particle particles;
  printf "@]@."
;;

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

let normalize positions =
  let sum = Array.fold_left (fun x (s, p) -> x +. s) 0. positions in
  if sum <= 0. then
    raise Lost
  else
    Array.iteri (fun i (s, p) -> positions.(i) <- (s /. sum, p)) positions
;;

let update robot positions get_distance =
  if delta_dist == 0. then
    raise (Invalid_argument "Null noise.");

  let new_weight score err =
    score *. exp ((-1. *. err *. err) /. (2. *. delta_dist *. delta_dist)) in

  let update_pos (sensor_pos, sensor) i (score, position) =
    let expected_dist = get_distance (add_position position sensor_pos) in
    if expected_dist != max_int then
      begin
        let err = sensor () - expected_dist in
        positions.(i) <- (new_weight score (float_of_int err), position)
      end in

  let update (sensor_pos, sensor) =
    if sensor () != max_int then
      Array.iteri (update_pos (sensor_pos, sensor)) positions in
  List.iter update robot.dist_sensors;
  normalize positions
;;



(* Explained sum of squares *)
let compute_ess positions =
  let n = float_of_int (Array.length positions) in
  if (n <= 0.) then
    raise Empty_particle_array;

  let sum = Array.fold_left
      (fun sum (w, p) ->
        let temp = n *. w -. 1. in
        sum +. (temp *. temp))
      0. positions in
  n /. (1. +. sum /. n)
;;


let resample positions =
  let n = Array.length positions in
  let q = Array.make n 0. in

  if n == 0 then
    raise Empty_particle_array;

  (* cumulative distribution *)
  Array.iteri
    (fun i (s, p) ->
      if i == 0 then
        q.(i) <- s
      else
        q.(i) <- q.(i - 1) +. s) positions;

  let x = Array.make n 0
  and index = Array.make n 0 in
  for i = 0 to pred n do
    x.(i) <- Random.int (n*2);
    for j = 0 to pred i do
      if x.(j) < x.(i) then
        begin
          if index.(i) <= index.(j) then
            index.(i) <- index.(j) + 1
        end
      else
        index.(j) <- index.(j) + 1
    done;
  done;

  let r = Array.make n 0. in
  for i = 0 to pred n do
    r.(index.(i)) <- (float_of_int x.(i)) /. (2. *. float_of_int n);
  done;

  let new_particles = Array.make n (0, 0, 0) in
  let i = ref 0 and j = ref 0 in
  while !i < pred n do
    if r.(!i) < q.(!j) then
      begin
        let (_, particle) = positions.(!j) in
        new_particles.(!i) <- particle;
        i := !i + 1
      end
    else
      j := !j + 1
  done;

  let weight = 1. /. (float_of_int n) in
  Array.iteri
    (fun i (s, p) ->
      positions.(i) <- (weight, new_particles.(i)))
    positions
;;


(* localize the robot using Monte Carlo Localization algorithm.
   Returns a list of possible position with a score. *)
let localize robot positions motion_model get_distance =
  let ess_threshold = 0.2 *. float_of_int (Array.length positions) in
  predict positions motion_model;

  update robot positions get_distance;

  let ess = compute_ess positions in
  if (ess < ess_threshold || ess == nan) then
    begin
      printf "Resample (ess = %f)@." ess;
      resample positions
    end
  else
    printf "Ess = %f@." ess;
;;
