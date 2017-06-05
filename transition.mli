type t = {
    lhs: ((int*int) list * Pattern.cc * Pattern.id) list option;
    rhs: ((int*int) list * Pattern.cc * Pattern.id) list;
    eid: int;
  }

val lhs : t -> ((int*int) list * Pattern.cc * Pattern.id) list option
val rhs : t -> ((int*int) list * Pattern.cc * Pattern.id) list
val eid : t -> int

val print : t -> Signature.s -> unit
val make :
  ((int*int) list * Pattern.cc * Pattern.id) list option ->
  ((int*int) list * Pattern.cc * Pattern.id) list -> int -> t
