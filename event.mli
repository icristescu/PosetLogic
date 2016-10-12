open Yojson

type quark =
  | Tested of (int*int*int)
  | Modified of (int*int*int)
  | TestedMod of (int*int*int)

type t = {
    event_id : int; (* local id inside a story *)
    event_label : string;
    quarks : quark list;
  }

val nodes_of_json : Yojson.Basic.json -> t

val get_id : t -> int
val get_label : t -> string

val print_event : t -> unit
val test_event : int -> string -> t
