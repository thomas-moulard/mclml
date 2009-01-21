(* Robot definition *)

type position = {
    x : int;
    y : int;
    theta : int;
  }
;;

type 'a sensor = unit -> 'a;;
type actuator = int -> unit;;

type distance_sensor = int sensor;;

type robot = {
    pos : position;
    dist_sensors : distance_sensor list;
    actuators : actuator list;
  }
;;
