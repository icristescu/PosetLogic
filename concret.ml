let linears s =
  LinearPoset.linearisations(s)

let concretise (s:LinearPoset.t) =
  let () = if (!Param.debug_mode) then
             (Format.printf "concretise linear poset :";
              List.iter (fun i -> Format.printf "%d " i) s.LinearPoset.seq) in
  let (trace,_) =
    List.fold_left
      (fun (trace,rf) eid ->
        let event = Poset.get_event_by_id eid s.LinearPoset.pos in
        let step = Event.get_step event in
        let m = Transition.get_rhs (TraceConcret.get_first_transition trace) in
        let (trans,new_nodes,new_ports) =
          Transition.make m step
                          trace.TraceConcret.node_names
                          trace.TraceConcret.port_names in
        ((TraceConcret.add_transition trace trans new_nodes new_ports),
         ((eid,trans)::rf)))
      (TraceConcret.empty,[]) s.LinearPoset.seq in
  trace


let concret (s:Poset.t) = concretise (linears s)
