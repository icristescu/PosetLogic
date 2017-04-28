open Yojson

type t = {
    event_id : int;
    rule_id : int;
    event_label : string;
    step : Trace.step ;
  }

val nodes_of_json : Model.t option -> Yojson.Basic.json -> t

val id : t -> int
val label : t -> string
val step : t -> Trace.step
val rule_id : t -> int

val print_event : t -> unit
val test_event : int -> string -> t
