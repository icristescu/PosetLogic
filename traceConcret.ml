
type s = Transition.s list

type t = Transition.t list

let empty = []

(*
let print t sigs = List.iter (fun trans -> Transition.print trans sigs) t
 *)

let get_first_transition t = List.hd t
let get_last_transition t = List.nth t ((List.length t)-1)
let add_transition trace trans = trans::trace

let pattern_trace contact_map sigs pre_env trace =
  List.map (fun trans ->
             Transition.pattern_transition contact_map sigs pre_env trans) trace
