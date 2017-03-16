let linears s =
  LinearPoset.linearisations(s)

let concretise s sigs =
  let () = if (!Param.debug_mode) then
             (Format.printf "concretise linear poset :";
              List.iter (fun i -> Format.printf "%d " i) s.LinearPoset.seq) in
  let init_state = Replay.init_state in
  let trace =
    List.fold_left
      (fun trace eid ->
        let event = Poset.get_event_by_id eid s.LinearPoset.pos in
        let step = Event.get_step event in
        let lhs_state =
          try Transition.get_rhs_state
                (TraceConcret.get_first_transition trace)
          with Failure s -> init_state in
        let (rhs_state,_) = Replay.do_step sigs lhs_state step in
        let trans = Transition.make lhs_state rhs_state in

        TraceConcret.add_transition trace trans)
      TraceConcret.empty s.LinearPoset.seq in
  trace

let concret s sigs = concretise (linears s) sigs
