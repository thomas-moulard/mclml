(* Demonstration software *)

open Format;;
open Graphics;;

open Geometry;;
open Mcl;;
open Mcl_render;;
open Render;;
open Robot;;
open Simulation;;


let win_width = 640;;
let win_height = 480;;
let win_box = make_box (0, 0) win_width win_height;;


let init () =
  Random.init 0;
  open_graph (sprintf " %dx%d" win_width win_height);
  set_window_title "Monte Carlo Localization demo";
  auto_synchronize false;
;;

let mouse_add_obstacle world point =
  let (x_prev, _) = !point
  and (xm, ym) = mouse_pos () in
  let mouse = (xm, win_height - ym) in
  let valid_position (x, y) =
    x >= 0 && x < win_width && y >= 0 && y < win_height
      && not (eq_point !point mouse)
      && x_prev >= -1 in
  begin
    match (x_prev, button_down () && valid_position mouse) with
    | (-2, _) -> if not (button_down ()) then point := (-1, -1)
    | (_, true) ->
        if x_prev < 0 then
          point := mouse
        else
          let line = (!point, mouse) in
          make_obstacle world line;
          point := (-2, -2);
    | (_, _) -> ()
  end;
;;

let print_sensors robot =
  let print_sensor ((x, y, theta), sensor) =
    let dist = sensor () in
    printf "Sensor (%d, %d, %d): " x y theta;
    if dist == max_int then
      printf "no obstacle.@."
    else
      printf "obstacle (distance: %d)@." dist in
  List.iter print_sensor robot.dist_sensors
;;

let render_all positions world robots =
  let surface = init_render_image win_box in
  render_mcl surface positions;
  render_world surface world robots;
  draw_render surface;
  synchronize ();
;;

let main () =
  init ();
  let obstacle = ref (-1, -1)
  and world = make_world [
    ((0, 0), (pred win_width, 0));
    ((0, 0), (0, pred win_height));
    ((0, pred win_height), (pred win_width, pred win_height));
    ((pred win_width, 0), (pred win_width, pred win_height));
  ] [] in

  (* Initialize robot *)
  let robots = [make_robot [] []] in

  let robot = List.nth robots 0 in
  robot.pos <- (win_width/2, win_height/2, 0);

  List.iter (add_robot world) robots;

  let robot_cfg = List.nth world.robots_cfg 0 in
  let robot = List.nth robots 0 in
  add_actuator robot (make_virtual_speed_actuator world robot_cfg (0, 0, 0));
  add_actuator robot (make_virtual_angle_actuator world robot_cfg (0, 0, 0));
  add_dist_sensor robot (0, 0, 0)
    (make_virtual_distance_sensor world robot_cfg (0, 0, 0));
  add_dist_sensor robot (0, 0, 90)
    (make_virtual_distance_sensor world robot_cfg (0, 0, 90));

  let speed_actuator = List.nth robot.actuators 1
  and angle_actuator = List.nth robot.actuators 0 in

  let motion_model linear_speed angular_speed position =
    virtual_robot_move position linear_speed angular_speed in
  let get_distance = intersect_position_obstacles world in

  let controller linear_speed angular_speed =
    speed_actuator linear_speed;
    angle_actuator angular_speed;
    motion_model linear_speed angular_speed in

  let _ = controller 1 1 in

  let positions = initialize_particles 100 robot.pos in

  (* Main loop *)
  let _ = read_key () in
  try
    while true do
      printf "+++ New step +++@.";
      printf "Robot (expected) position: ";
      print_position robot.pos;
      printf "@.";

      print_sensors robot;
      mouse_add_obstacle world obstacle;
      update_world world;

      localize robot positions (controller 1 1) get_distance;

      render_all  positions world robots
    done;
  with Graphic_failure _ -> close_graph ()
  | Lost ->
      printf "******************@.";
      printf "* Robot is lost! *@.";
      printf "******************@.";
      print_sensors robot;
      print_mcl_particles positions;
      while true do
        render_all  positions world robots
      done
;;

main ()
