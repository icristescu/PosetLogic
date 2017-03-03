open Yojson

(*module IntMap : Map.S with type key = int*)

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
val get_quarks : t -> quark list

val print_event : t -> unit
val test_event : int -> string -> t

val quarks_tested : quark list -> quark list
val quarks_testedMod : quark list -> quark list
val quarks_modified : quark list -> quark list
val get_nodes : quark list -> int list
val get_nodes_ports : quark list -> (int, (int*int list)) Hashtbl.t
