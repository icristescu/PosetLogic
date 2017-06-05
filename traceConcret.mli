type t = Transition.t list

val empty: t
val print: t -> Signature.s -> unit

val get_last_lhs_graph:
  t -> ((int*int) list * Pattern.cc * Pattern.id) list option

val add_transition:
  Signature.s -> t ->
  ((int*int) list * Pattern.cc * Pattern.id) list -> int -> t
