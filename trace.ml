
type t = {
    transitions : Transition.t list;
    node_names : (int*string) list;
    port_names : (string * (int* string) list) list;
  }

let empty = {transitions = [];node_names=[];port_names=[]}

let print t = List.iter (fun trans -> Transition.print trans) t.transitions

let get_first_transition t =
  try
    List.hd t.transitions
  with Failure s -> Transition.empty

let get_last_transition t =
    List.nth t.transitions ((List.length t.transitions)-1)

let add_transition trace trans new_nodes new_ports =
  {transitions=trans::trace.transitions;
   node_names=new_nodes@trace.node_names;
   port_names=new_ports@trace.port_names;}
