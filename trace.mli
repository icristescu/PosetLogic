
type t = {
    transitions : Transition.t list;
  }

val empty: t
val print: t -> unit

val get_last_transition: t -> Transition.t
val get_first_transition: t -> Transition.t
val add_transition: t -> Transition.t -> t
