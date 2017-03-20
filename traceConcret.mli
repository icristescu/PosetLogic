type s = Transition.s list
type t = Transition.t list


val empty: s
val print: t -> Signature.s -> unit

val get_last_transition: s -> Transition.s
val get_first_transition: s -> Transition.s
val add_transition: s -> Transition.s -> s
val pattern_trace: Signature.s -> Contact_map.t -> s -> t
val get_last_context: t -> Pattern.cc list
