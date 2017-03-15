open Yojson

type t = {
    event_id : int; (* local id inside a story *)
    event_label : string;
    step : Trace.step ;
  }

val nodes_of_json : Model.t -> Yojson.Basic.json -> t

val get_id : t -> int
val get_label : t -> string
val get_step : t -> Trace.step

val print_event : t -> unit
val test_event : int -> string -> t
