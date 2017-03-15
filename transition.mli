type t = {
    lhs: Idgraph.mixture;
    rhs: Idgraph.mixture;
  }

val empty : t
val print : t -> unit
val get_rhs : t -> Idgraph.mixture
val make : Idgraph.mixture -> Trace.step -> Maps.node_map -> Maps.port_map ->
           t * Maps.node_map * Maps.port_map
