(* Generic geometry related stuff *)

open Graphics;;
open Format;;

let pi = 3.1415926535897932384626433832795028841971693992;;

type point = int * int;;

type position = int * int * int;;

type line = point * point;;

type box = {
    p : point;
    width : int;
    height : int;
  }
;;

let add_position (x1, y1, theta1) (x2, y2, theta2) =
  (x1 + x2, y1 + y2, theta1 + theta2)
;;

let eq_point (x1, y1) (x2, y2) =
  x1 == x2 && y1 == y2
;;

let wrap_angle = (mod) 360;;


let euclidian_distance (x1, y1) (x2, y2) =
  let dx = x1 - x2 and dy = y1 - y2 in
  int_of_float (sqrt (float_of_int (dx * dx + dy * dy)))
;;

let gradient_of_degree x = x *. 2. *. pi /. 360.;;

let print_point (x, y) =
  printf "@[(%d,@ %d)@]" x y
;;

let print_line (p1, p2) =
  printf "@[";
  print_point p1;
  printf "@ ";
  print_point p2;
  printf "@]"
;;

let make_box pt w h = {
  p = pt;
  width = w;
  height = h
};;

let get_point surface x y = surface.(y).(x)

let draw_point surface color (x, y) =
  let ydim = Array.length surface
  and xdim = Array.length surface.(0) in

  if x >= 0 && y >= 0 && x < xdim && y < ydim then
    surface.(y).(x) <- color;
;;


(* Middle-point algorithm *)
let middle_point_algorithm (p1, p2) point_fun =
  let (_, y1) = p1 and (_, y2) = p2 in
  let ((x_low, y_low), (x_high, y_high)) =
    if y1 < y2 then
      (p1, p2)
    else
      (p2, p1) in
  let dy = y_high - y_low in

  let apply op d0 deltaE deltaNE chooseE chooseNE =
    let x = ref x_low and y = ref y_low and dp = ref d0 in
    point_fun (x_low, y_low);
    while op (!x) x_high do
      if (!dp <= 0) then
        begin
          dp := !dp + deltaE;
          chooseE x y
        end
      else
        begin
          dp := !dp + deltaNE;
          chooseNE x y
        end;
      point_fun (!x, !y);
    done;
    if !x == x_high && !y != y_high then
      begin
        point_fun (!x, !y);
        while (!y < y_high) do
          y := !y + 1;
          point_fun (!x, !y);
        done;
      end
  in

  match x_high >= x_low with
  | true ->
      begin
        let dx = x_high - x_low in
        match dx >= dy with
        | true ->
            apply (<) (2*dy-dx) (2*dy) (2*(dy-dx))
              (fun x y -> x := !x + 1)
              (fun x y -> x := !x + 1; y := !y + 1)
        | false ->
            apply (<) (2*dx-dy) (2*dx) (2*(dx-dy))
              (fun x y -> y := !y + 1)
              (fun x y -> x := !x + 1; y := !y + 1)
      end
  | false ->
      let dx = x_low - x_high in
      match dx >= dy with
      | true ->
          apply (>) (2*dy-dx) (2*dy) (2*(dy-dx))
            (fun x y -> x := !x - 1)
            (fun x y -> x := !x - 1; y := !y + 1)
      | false ->
          apply (>) (2*dx-dy) (2*dx) (2*(dx-dy))
            (fun x y -> y := !y + 1)
            (fun x y -> x := !x - 1; y := !y + 1)
;;

let points_from_line line =
  let res = ref [] in
  middle_point_algorithm line (fun p -> res := p::(!res));
  !res
;;

let draw_line color line surface =
  middle_point_algorithm line (draw_point surface color)
;;

(* Normalize to avoid stable positions for low speeds *)
let floor_ceil xold x =
  if x > xold then
    ceil x
  else
    floor x
;;

let point_from_position (x, y, theta) r =
  let t = gradient_of_degree (float_of_int theta) in
  let x_ = (cos t) *. (float_of_int r) +. float_of_int x
  and y_ = (sin t) *. (float_of_int r) +. float_of_int y in
  (int_of_float (floor_ceil (float_of_int x) x_),
   int_of_float (floor_ceil (float_of_int y) y_))
;;

let draw_circle surface color (x, y) r =
  let step = 1. /. (float_of_int r)
  and i = ref 0. in
  while !i <= (2. *. pi) do
    let x_ = (cos !i) *. (float_of_int r) +. float_of_int x
    and y_ = (sin !i) *. (float_of_int r) +. float_of_int y in
    draw_point surface color
      (int_of_float (floor_ceil (float_of_int x) x_),
       int_of_float (floor_ceil (float_of_int y) y_));
    i := !i +. step;
  done
;;


let draw_position surface color (x, y, theta) r =
  draw_circle surface color (x, y) r;
  let p = point_from_position (x, y, theta) r in
  draw_line color ((x, y), p) surface
;;


(* Return (a, b, c) such that a*x+b*y=c *)
let get_line_coeff ((x1, y1), (x2, y2)) =
  let a = y2 - y1
  and b = x1 - x2 in
  (a, b, a * x1 + b * y1)
;;

exception No_intersection;;


let intersect_lines l1 l2 =
  let (a1, b1, c1) = get_line_coeff l1
  and (a2, b2, c2) = get_line_coeff l2 in

  let det = a1*b2 - a2*b1 in

  if det == 0 then
    raise No_intersection
  else
    ((b2*c1 - b1*c2)/det, (a1*c2 - a2*c1)/det)
;;

let in_segment (x, y) ((x0, y0), (x1, y1)) =
  let xmin = min x0 x1
  and xmax = max x0 x1
  and ymin = min y0 y1
  and ymax = max y0 y1 in
  xmin <= x && x <= xmax
    && ymin <= y && y <= ymax
;;

let intersect_position_line (x, y, theta) s =
  let (x_, y_) = point_from_position (x, y, theta) 42 in
  let (xi, yi) = intersect_lines s ((x_, y_), (x, y)) in

  if not (in_segment (xi, yi) s) then
    raise No_intersection;

  let good_res =
    match (x < x_, y < y_) with
    | (true, true) -> x < xi && y < yi
    | (true, false) -> x < xi && y >= yi
    | (false, true) -> x >= xi && y < yi
    | (false, false) -> x >= xi && y >= yi in
  if good_res then
    (xi, yi)
  else
    raise No_intersection
;;
