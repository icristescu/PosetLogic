type t = {
    lhs: Pattern.cc list;
    rhs: Pattern.cc list;
  }

type s = {
    lhs_state: Replay.state;
    rhs_state: Replay.state;
  }

val empty : Signature.s -> t
val print : t -> Signature.s -> unit

val get_rhs_state : s -> Replay.state
val make : Replay.state -> Replay.state -> s

val pattern_transition:
  (int list * (int * int) list) array array -> Signature.s ->
  Pattern.PreEnv.t -> s -> t
