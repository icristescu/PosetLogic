type t = {
    lhs: ((int*int) list * Pattern.cc * Pattern.id) list option;
    rhs: ((int*int) list * Pattern.cc * Pattern.id) list option;
    eid: int;
  }

val print : t -> Signature.s -> unit
val print_cc : Signature.s -> Pattern.cc -> unit
val print_side :
  Signature.s -> ((int*int) list * Pattern.cc * Pattern.id) list option -> unit
val make :
  ((int*int) list * Pattern.cc * Pattern.id) list option ->
  ((int*int) list * Pattern.cc * Pattern.id) list option -> int -> t
