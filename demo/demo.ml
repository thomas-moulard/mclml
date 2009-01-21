(* Demonstration software *)

open Format;;
open Graphics;;

open Geometry;;
open Render;;
open Simulation;;

let win_width = 640;;
let win_height = 480;;
let win_box = make_box (0, 0) win_width win_height;;


let init () =
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

let main () =
  init ();
  let obstacle = ref (-1, -1)
  and world = make_world [
    ((0, 0), (pred win_width, 0));
    ((0, 0), (0, pred win_height));
    ((0, pred win_height), (pred win_width, pred win_height));
    ((pred win_width, 0), (pred win_width, pred win_height));
  ] in
  while true do
    mouse_add_obstacle world obstacle;
    draw_image (render world win_box) 0 0;
    synchronize ();
  done;
  close_graph ()
;;

main ()
