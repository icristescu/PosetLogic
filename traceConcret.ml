
type s = Transition.s list

type t = Transition.t list

let empty = []

let get_first_transition t = List.hd t
let get_last_transition t = List.nth t ((List.length t)-1)
let add_transition trace trans = trans::trace

let pattern_trace sigs contact_map trace =
  let () = if (!Param.debug_mode) then Format.printf "pattern of trace\n" in
  List.map (fun trans ->
             Transition.pattern_transition sigs contact_map trans) trace

let print t sigs =
  List.iter
    (fun trans ->
      Transition.print_side trans.Transition.lhs sigs; Format.printf " => ") t;
  let trans = get_last_transition t in
  Transition.print_side trans.Transition.rhs sigs

let get_last_context t =
  let trans = get_last_transition t in
  trans.Transition.rhs
