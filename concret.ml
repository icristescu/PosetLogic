let linears s =
  LinearPoset.linearisations(s)

let concretise (s:LinearPoset.t) (rs:Rule.t list) =
  let () = if (!Parameter.debug_mode) then
             (Format.printf "concretise linear poset :";
              List.iter (fun i -> Format.printf "%d " i) s.LinearPoset.seq) in
  let (trace,_) =
    List.fold_left
      (fun (trace,rf) eid ->
        let event = Poset.get_event_by_id eid s.LinearPoset.pos in
        let rname = Event.get_label event in
        let rule = Rule.get_rule_by_label rname rs in
        let m = Transition.get_rhs (Trace.get_first_transition trace) in
        let (trans,new_nodes,new_ports) =
          Transition.make m rule (Event.get_quarks event)
                          trace.Trace.node_names trace.Trace.port_names in
        ((Trace.add_transition trace trans new_nodes new_ports),
         ((eid,trans)::rf)))
      (Trace.empty,[]) s.LinearPoset.seq in
  trace

(*let test_concret (s:LinearPoset.t) (rs:Rule.t list) =
  let () = if (!Parameter.debug_mode) then
             (Format.printf "concretise linear poset :";
              List.iter (fun i -> Format.printf "%d " i) s.LinearPoset.seq) in
  let event = Poset.get_event_by_id 4 s.LinearPoset.pos in
  let rname = Event.get_label event in
  let rule = Rule.get_rule_by_label rname rs in

  let () = if (!Parameter.debug_mode) then
             (Format.printf "info on first event ";
              Event.print_event event;
              Rule.print rule) in

  let (trans,_,_) = Transition.make [] rule (Event.get_quarks event) [] [] in
  let () = if (!Parameter.debug_mode) then
              Transition.print trans in
  Trace.empty
 *)

let concret (s:Poset.t) (rs:Rule.t list) = concretise (linears s) rs
