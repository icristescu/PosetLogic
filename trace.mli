
type t = {
    transitions : Transition.t list;
    node_names : Maps.node_map;
    port_names : Maps.port_map;
  }

val empty: t
val print: t -> unit

val get_last_transition: t -> Transition.t
val get_first_transition: t -> Transition.t
val add_transition: t -> Transition.t -> Maps.node_map ->
                     Maps.port_map ->t
