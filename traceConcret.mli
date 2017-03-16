type s = Transition.s list
type t = Transition.t list


val empty: s
(*val print: t -> Signature.s -> unit*)

val get_last_transition: s -> Transition.s
val get_first_transition: s -> Transition.s
val add_transition: s -> Transition.s -> s
val pattern_trace:
  (int list * (int * int) list) array array -> Signature.s ->
  Pattern.PreEnv.t -> s -> t
