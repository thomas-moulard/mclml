(* Robot definition *)

type position = int * int * int;;

type 'a sensor = unit -> 'a;;
type actuator = int -> unit;;

type distance_sensor = int sensor;;

(* FIXME: simplification one actuator which defines speed *)
type robot = {
    mutable pos : position;
    mutable dist_sensors : distance_sensor list;
    mutable actuators : actuator list;
  }
;;

let make_robot dist_sensors_list actuators_list = {
  pos = (0, 0, 0);
  dist_sensors = dist_sensors_list;
  actuators = actuators_list
};;

let add_actuator robot actuator =
  robot.actuators <- actuator::robot.actuators
;;

let add_dist_sensor robot sensor =
  robot.dist_sensors <- sensor::robot.dist_sensors
;;
