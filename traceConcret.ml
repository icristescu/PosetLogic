
type t = Transition.t list

let empty = []

let last_transition t = List.nth t ((List.length t)-1)

let add_transition sigs trace rhs_graph eid =
  let lhs_graph =
    try
      let last_trans = List.hd trace in (Some (Transition.rhs last_trans))
    with Failure _ -> None in
  let trans = Transition.make lhs_graph rhs_graph eid in
  trans::trace

let print t sigs = List.iter (fun trans -> Transition.print trans sigs) t

let get_last_lhs_graph t =
  Transition.lhs (last_transition t)
