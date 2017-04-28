
type t = Transition.t list

let empty = []

let last_transition t = List.nth t ((List.length t)-1)

let add_transition sigs trace rhs_graph eid =
  let lhs_graph =
    try
      let last_trans = List.hd trace in last_trans.Transition.rhs
    with Failure _ -> None in
  let trans = Transition.make lhs_graph rhs_graph eid in
  trans::trace

let last_context t =
  let trans = last_transition t in
  trans.Transition.lhs

let last_rule_id t poset =
  let trans = last_transition t in
  let eid = trans.Transition.eid in
  let ev = Poset.get_event_by_id eid poset in
  let rid = Event.rule_id ev in
  rid

let print t sigs =
  List.iter
    (fun trans ->
      Transition.print_side sigs trans.Transition.lhs; Format.printf " => ") t;
  let trans = last_transition t in
  Transition.print_side sigs trans.Transition.rhs
