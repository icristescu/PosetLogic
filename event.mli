open Yojson

type t = {
    event_id : int; (* local id inside a story *)
    event_label : string;
    quarks : Quark.t list;
  }

val nodes_of_json : Yojson.Basic.json -> t

val get_id : t -> int
val get_label : t -> string
val get_quarks : t -> Quark.t list

val print_event : t -> unit
val test_event : int -> string -> t
