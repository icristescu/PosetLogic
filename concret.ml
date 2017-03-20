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

        let () = if (!Param.debug_mode) then
                   (Format.printf "concret of event\n";
                    Event.print_event event) in

        let lhs_state =
          try Transition.get_rhs_state
                (TraceConcret.get_first_transition trace)
          with Failure s -> init_state ~with_connected_components:false in

        let () = if (!Param.debug_mode) then
                   (Format.printf "lhs_state\n";
                    Edges.debug_print (Format.std_formatter)
                                      (lhs_state.Replay.graph)) in
        let lhs_copy = Transition.copy_state lhs_state in
        let (rhs_state,_) = Replay.do_step sigs lhs_copy step in

        let () = if (!Param.debug_mode) then
                   (Format.printf "rhs_state\n";
                    Edges.debug_print (Format.std_formatter)
                                      (rhs_state.Replay.graph)) in

        let trans = Transition.make lhs_state rhs_state eid in

        TraceConcret.add_transition trace trans)
      TraceConcret.empty s.LinearPoset.seq in
  trace

let concret s sigs =
  let trace = concretise (linears s) sigs in
  List.rev trace

let context_of_application s1 s2 sigs env =
  let contact_map = Model.contact_map env in

  let t1 = concret s1 sigs in
  let pt1 = TraceConcret.pattern_trace sigs contact_map t1 in
  let graph1 = TraceConcret.get_last_context pt1 in

  let t2 = concret s2 sigs in
  let pt2 = TraceConcret.pattern_trace sigs contact_map t2 in
  let graph2 = TraceConcret.get_last_context pt2 in

  let () = if (!Param.debug_mode) then
             (TraceConcret.print pt1 sigs;TraceConcret.print pt2 sigs) in
  let () = Format.printf "@.trace1 = ";TraceConcret.print pt1 sigs;
           Format.printf "@.trace2 = ";TraceConcret.print pt2 sigs;
           Format.printf "@.graph1 = "; Transition.print_side graph1 sigs ;
           Format.printf "@.graph2 = "; Transition.print_side graph2 sigs in
  let g1 = List.hd graph1 in
  let g2 = List.hd graph2 in
  let cc_list = Pattern.infs g1 g2 in
  let () = Format.printf "@.the infs =";
           List.iter (fun cc -> Pattern.print_cc ~new_syntax:true ~sigs:sigs
                                                 ~with_id:true
                                                 (Format.std_formatter) cc;
                                Format.printf ";;   ")
                     cc_list in
  let () =
    List.iter
      (fun cc ->
        List.iter (fun m ->
                    Format.printf "@.matching %a@." Renaming.print_full m;
                    (*let pushout = Pattern.pushout m g1 g2 in
                    Format.printf "pushout =";
                    Transition.print_side [pushout] sigs*))
                  (Pattern.matchings cc g1)) cc_list in
  ()
