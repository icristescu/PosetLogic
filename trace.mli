
type t = {
    transitions : Transition.t list;
    node_names : (int*string) list;
    port_names : (string * (int* string) list) list;
  }

val empty: t
val print: t -> unit

val get_last_transition: t -> Transition.t
val get_first_transition: t -> Transition.t
val add_transition: t -> Transition.t -> (int*string) list ->
                    (string * (int* string) list) list ->t
