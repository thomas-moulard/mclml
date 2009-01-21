(* Generic geometry related stuff *)

open Graphics;;
open Format;;

type point = int * int;;

let eq_point (x1, y1) (x2, y2) =
  x1 == x2 && y1 == y2
;;

type line = point * point;;

type box = {
    p : point;
    width : int;
    height : int;
  }
;;

let printpoint (x, y) =
  printf "@[(%d,@ %d)@]" x y
;;

let printline (p1, p2) =
  printf "@[";
  printpoint p1;
  printf "@ ";
  printpoint p2;
  printf "@]"
;;

let make_box pt w h = {
  p = pt;
  width = w;
  height = h
};;

let get_point surface x y = surface.(y).(x)

let print_point surface color (x, y) =
  surface.(y).(x) <- color;
;;

(* Middle-point algorithm *)
let print_line color (p1, p2) surface =
  let (_, y1) = p1 and (_, y2) = p2 in
  let ((x_low, y_low), (x_high, y_high)) =
    if y1 < y2 then
      (p1, p2)
    else
      (p2, p1) in
  let dy = y_high - y_low in

  let apply op d0 deltaE deltaNE chooseE chooseNE =
    let x = ref x_low and y = ref y_low and dp = ref d0 in
    print_point surface color (x_low, y_low);
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
      print_point surface color (!x, !y);
    done;
    if !x == x_high && !y != y_high then
      begin
        print_point surface color (!x, !y);
        while (!y < y_high) do
          y := !y + 1;
          print_point surface color (!x, !y);
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
