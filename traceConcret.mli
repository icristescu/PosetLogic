type t = Transition.t list

val empty: t
val print: t -> Signature.s -> unit
val last_transition: t -> Transition.t
val add_transition:
  Signature.s -> t ->
  ((int*int) list * Pattern.cc * Pattern.id) list option -> int -> t
