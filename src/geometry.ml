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
let print_line color line surface =
  let ((x1, y1), (x2, y2)) = line in
  let dy = y2 - y1 in

  let apply d0 deltaE deltaNE chooseE chooseNE =
    let x = ref x1 and y = ref y1 and dp = ref d0 in
    print_point surface color (x1, y1);
    while (!x < x2) do
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
    if !x == x2 && !y != y2 then
      begin
        print_point surface color (!x, !y);
        while (!y < y2) do
          y := !y + 1;
          print_point surface color (!x, !y);
        done;
      end
  in

  match x2 >= x1 with
  | true ->
      begin
        let dx = x2 - x1 in
        match dx >= dy with
        | true ->
            apply (2*dy-dx) (2*dy) (2*(dy-dx))
              (fun x y -> x := !x + 1)
              (fun x y -> x := !x + 1; y := !y + 1)
        | false ->
            apply (2*dx-dy) (2*dx) (2*(dx-dy))
              (fun x y -> x := !x + 1)
              (fun x y -> x := !x + 1; y := !y + 1)
      end
  | false ->
      let dx = x1 - x2 in
      match dx >= dy with
      | true ->
          apply (2*dy-dx) (2*dy) (2*(dy-dx))
            (fun x y -> x := !x + 1)
            (fun x y -> x := !x + 1; y := !y + 1)
      | false ->
          apply (2*dx-dy) (2*dx) (2*(dx-dy))
            (fun x y -> x := !x + 1)
            (fun x y -> x := !x + 1; y := !y + 1)
;;
