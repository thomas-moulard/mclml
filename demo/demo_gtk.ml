open GMain
open GObj
open GdkKeysyms

open Geometry;;
open Mcl;;
open Mcl_render;;
open Render;;
open Robot;;
open Simulation;;

let locale = GtkMain.Main.init ();;

let width = 640 and height = 480;;

(* Backing pixmap for drawing area *)
let backing = ref (GDraw.pixmap ~width ~height ())

let render_all particles world robots =
  let surface = init_render_image (make_box (0, 0) width height) in
  render_mcl surface particles;
  render_world surface world robots;
  surface

let update_drawing_area area (backing:GDraw.pixmap ref) world particles robots =
  let rgb_to_comps pixel =
    ((pixel lsr 16) land 255,
     (pixel lsr 8) land 255,
     pixel land 255) in

  let robot = List.nth robots 0 in
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

  update_world world;
  localize robot particles (controller 1 1) get_distance;
  let surface = render_all particles world robots in

  let update_rect = Gdk.Rectangle.create 0 0 width height in

  for y = 0 to pred (Array.length surface) do
    for x = 0 to pred (Array.length surface.(y)) do
      let (r,g,b) = rgb_to_comps surface.(y).(x) in
      !backing#set_foreground (`RGB (r*256, g*256, b*256));
      !backing#point x y
    done
  done;
  area#misc#draw (Some update_rect)
;;

(* Create a new backing pixmap of the appropriate size *)
let configure window backing ev =
  let width = GdkEvent.Configure.width ev in
  let height = GdkEvent.Configure.height ev in
  let pixmap = GDraw.pixmap ~width ~height ~window () in
  pixmap#set_foreground `WHITE;
  pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  backing := pixmap;
  true

(* Redraw the screen from the backing pixmap *)
let expose (drawing_area:GMisc.drawing_area) (backing:GDraw.pixmap ref) ev =
  let area = GdkEvent.Expose.area ev in
  let x = Gdk.Rectangle.x area in
  let y = Gdk.Rectangle.y area in
  let width = Gdk.Rectangle.width area in
  let height = Gdk.Rectangle.width area in
  let drawing =
    drawing_area#misc#realize ();
    new GDraw.drawable (drawing_area#misc#window)
  in
  drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !backing#pixmap;
  false


let main () =

  let world = make_world [
    ((0, 0), (pred width, 0));
    ((0, 0), (0, pred height));
    ((0, pred height), (pred width, pred height));
    ((pred width, 0), (pred width, pred height));
  ] [] in

  let robots = [make_robot [] []] in

  let robot = List.nth robots 0 in
  robot.pos <- (width/2, height/2, 0);

  List.iter (add_robot world) robots;

  let robot_cfg = List.nth world.robots_cfg 0 in
  let robot = List.nth robots 0 in
  add_actuator robot (make_virtual_speed_actuator world robot_cfg (0, 0, 0));
  add_actuator robot (make_virtual_angle_actuator world robot_cfg (0, 0, 0));
  add_dist_sensor robot (0, 0, 0)
    (make_virtual_distance_sensor world robot_cfg (0, 0, 0));
  add_dist_sensor robot (0, 0, 90)
    (make_virtual_distance_sensor world robot_cfg (0, 0, 90));

  let particles = initialize_particles 100 robot.pos in

  let window = GWindow.window ~width:640 ~height:510
      ~title:"MclML GTK2 Demo" () in
  let vbox = GPack.vbox ~packing:window#add () in
  let _ = window#connect#destroy ~callback:Main.quit in

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  let _ = factory#add_item "Quit" ~key:_Q ~callback: Main.quit in

  (* Simulation zone. *)
  let area = GMisc.drawing_area ~width ~height ~packing:vbox#add () in
  let _ = area#event#connect#expose ~callback:(expose area backing) in
  let _ = area#event#connect#configure ~callback:(configure window backing) in
  let _ = area#event#add [`EXPOSURE] in

  let _ = GMain.Timeout.add ~ms:500
      ~callback: (fun _ -> update_drawing_area area backing world particles robots; true) in
  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()
;;

main ()
