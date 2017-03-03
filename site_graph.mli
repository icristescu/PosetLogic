type node = {
    agent : int;
    agent_type : int;
    site : int;
  }

type edge =
  | Bound of node * node
  | Free of node

type internal_state = {
    agent_site : node;
    property : int;
  }

type t = {
    nodes : node list;
    edges : edge list;
    property_set : internal_state list;
  }

val print_graph : t -> unit
(*val test : unit -> unit*)

val empty: t
