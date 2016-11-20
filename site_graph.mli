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

type site_graph = {
    nodes : node list;
    edges : edge list;
    property_set : internal_state list;
  }

val print_graph : site_graph -> unit
val test : unit -> unit
