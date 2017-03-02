
type t = {
    source : Site_graph.t;
    dest : Site_graph.t;
    rule : Rule.t;
    node_map : (int*int) list;
  }
