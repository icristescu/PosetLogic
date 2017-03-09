type t = {
    lhs: Idgraph.mixture;
    rhs: Idgraph.mixture;
    rule : Rule.t;
  }

val empty : t
val print : t -> unit
val get_rhs : t -> Idgraph.mixture
val make : Idgraph.mixture -> Rule.t -> Quark.t list -> (int*string) list ->
                    (int * (int* string) list) list ->
                    t*Maps.node_map*Maps.port_map
