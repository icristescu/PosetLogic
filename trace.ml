
type t = {
    transitions : Transition.t list;
  }

let empty = {transitions = [];}

let print t = List.iter (fun trans -> Transition.print trans) t.transitions

let get_first_transition t =
  try
    List.hd t.transitions
  with Failure s -> Transition.empty

let get_last_transition t =
    List.nth t.transitions ((List.length t.transitions)-1)

let add_transition trace trans =
  {transitions=trans::trace.transitions}
