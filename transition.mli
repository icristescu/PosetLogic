type t = {
    lhs: Pattern.cc list;
    rhs: Pattern.cc list;
    eid: int;
  }

type s = {
    lhs_state: Replay.state;
    rhs_state: Replay.state;
    eid_state: int;
  }

val empty : Signature.s -> t
val print : t -> Signature.s -> unit
val print_side : Pattern.cc list -> Signature.s -> unit

val get_rhs_state : s -> Replay.state
val make : Replay.state -> Replay.state -> int -> s

val pattern_transition: Signature.s -> Contact_map.t -> s -> t
val copy_state: Replay.state ->  Replay.state
val copy: s ->  s
