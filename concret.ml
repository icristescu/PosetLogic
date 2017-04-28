(******* printing functions for testing *)
let print_replay_states event lhs_state rhs_state =
  Format.printf "@.concret of event";Event.print_event event;
  Format.printf "lhs_state@.";
  Edges.debug_print (Format.std_formatter)
                    (lhs_state.Replay.graph);
  Format.printf "rhs_state@.";
  Edges.debug_print (Format.std_formatter)
                    (rhs_state.Replay.graph)

let print_ac sigs tests actions =
  Format.printf "@.concrete_tests = ";
  List.iter
    (fun test ->
      Format.printf "%a; "(Instantiation.print_concrete_test ~sigs) test)
    tests;
  Format.printf "@.concrete_actions = ";
  List.iter
    (fun act ->
      Format.printf"%a; "(Instantiation.print_concrete_action ~sigs) act)
    actions

let print_context sigs graph lhs m n id =
  Format.printf "lhs%d = " id;Transition.print_side sigs (Some lhs);
  Format.printf "graph%d = " id;Transition.print_side sigs (Some graph);
  Format.printf "@.matching rule_cc_abstract= ";
  List.iter (fun (ls,_) ->
              Format.printf "cc: ";
              List.iter (fun (a,b) -> Format.printf "(%d,%d) " a b) ls) n;
  Format.printf "@.matching abstract_concrete =%a@." Renaming.print_full m

let print_infs sigs inf m2' l1 l2 =
  Format.printf "@.l1 = ";Transition.print_side sigs (Some [l1]);
  Format.printf "@.l2 = ";Transition.print_side sigs (Some [l2]);
  Format.printf "@.inf @.";Transition.print_cc sigs inf;
  Format.printf "@.m2' = %a " Renaming.print_full m2'

let print_build_context sigs m sup =
  (match sup with
  | Some s -> Format.printf "sup = ";Transition.print_cc sigs s
  | None -> Format.printf "@.No sup ");
  Format.printf "@.m2'' = %a " Renaming.print_full m

let print_morphisms rev_mg1 rev_m1 rev_n1 compose1 partial1 compose2 =
  Format.printf "@.rev_mg1 = ";
  List.iter (fun (a,b)-> Format.printf "(%d,%d) " a b) rev_mg1;
  Format.printf "@.rev_m1 = ";
  List.iter (fun (a,b)-> Format.printf "(%d,%d) " a b) rev_m1;
  Format.printf "@.rev_n1 = ";
  List.iter (fun (a,b)-> Format.printf "(%d,%d) " a b) rev_n1;
  Format.printf "@.compose1 = ";
  List.iter (fun (a,b)-> Format.printf "(%d,%d) " a b) compose1;
  Format.printf "@.partial1 = ";
  List.iter (fun (a,b)-> Format.printf "(%d,%d) " a b) partial1;
  Format.printf "@.compose2 = ";
  List.iter (fun (a,b)-> Format.printf "(%d,%d) " a b) compose2


(*********** functions on morphisms as lists *)
let list_to_renaming ls =
  match
    (List.fold_left
       (fun acc (a,b) ->
         match acc with
         | Some r -> Renaming.add a b r
         | None -> None) (Some Renaming.empty) ls) with
  | Some r -> r
  | None -> raise(ExceptDefn.Internal_Error("cannot construct renaming"))

let image m = List.map (fun (_,b) -> b) m
let domain m = List.map (fun (a,_) -> a) m

let compose m1 m2 =
  List.fold_left
    (fun acc (a1,b1) ->
      let (_,b2) = List.find (fun (a,_) -> (b1=a)) m2 in
      (a1,b2)::acc) [] m1

let reverse m = List.map (fun (a,b) -> (b,a)) m

(***********)
let linears s =
  LinearPoset.linearisations(s)

let copy_state s = {
    Replay.graph = (Edges.copy s.Replay.graph); time=s.Replay.time;
    event = s.Replay.event;connected_components=s.Replay.connected_components;}

let make sigs env replay_state =
  let (env',list) = Replay.cc_of_state replay_state env in
  (*  let () =
    if (!Param.debug_mode) then
      (Format.printf "make@.";Transition.print_side sigs (Some list)) in*)
  (env',(Some list))

let concretise s sigs =
  let () = if (!Param.debug_mode) then
             (Format.printf "concretise linear poset :";
              List.iter (fun i -> Format.printf "%d " i) s.LinearPoset.seq) in
  let init_state = Replay.init_state ~with_connected_components:true in
  let (trace,_,_) =
    List.fold_left
      (fun (trace,lhs_state,env) eid ->
        let event = Poset.get_event_by_id eid s.LinearPoset.pos in
        let step = Event.step event in
        let rhs_copy = copy_state lhs_state in
        let (rhs_state,_) = Replay.do_step sigs rhs_copy step in
        (* let () = if (!Param.debug_mode) then *)
        (*            print_replay_states event lhs_state rhs_state in *)
        let (env',graph) = make sigs env rhs_state in
        let trace' = TraceConcret.add_transition sigs trace graph eid in
        (trace',rhs_state,env'))
      (TraceConcret.empty,init_state,(Pattern.PreEnv.empty sigs))
      s.LinearPoset.seq in
  (List.rev trace)

let concret s sigs =
  concretise (linears s) sigs

let negative_influence inf = true

let compose_matchings mg1 m1 n1 m2' n2 m2 mg2 =
  let reverse_and_filter n m =
    reverse (List.filter (fun (a,b) -> (List.mem b (image n))) m) in
  let rev_mg1 = reverse mg1 in
  let rev_m1 = reverse_and_filter rev_mg1 (Renaming.to_list m1) in
  let rev_n1 = reverse_and_filter rev_m1 n1 in
  let compose1 = compose (compose rev_mg1 rev_m1) rev_n1 in
  let list_m2' = Renaming.to_list m2' in
  (* partial1: g1 -> inf *)
  let partial1 =
    List.filter (fun (a1,b1) -> List.mem b1 (domain list_m2')) compose1 in
  let list_m2 = Renaming.to_list m2 in
  let compose2 =
    compose (compose (compose (compose partial1 list_m2') n2) list_m2) mg2 in
  (* let () = *)
  (*   if (!Param.debug_mode) then *)
  (*     print_morphisms rev_mg1 rev_m1 rev_n1 compose1 partial1 compose2 in *)
  list_to_renaming compose2

let build_context
      sigs domain (n1,cc1,id1) (n2,cc2,id2) graph1 graph2 m1 n1 m2 n2 m2'=
  let find_cc id n m graph =
    let (n',_) = List.find (fun (_,i) -> (i=id)) n in
    let (rule_id,abstract_id) = List.hd n' in
    let concrete_id = Renaming.apply m abstract_id in
    let (mg,g,_) =
      List.find
        (fun (m,_,_) -> List.exists (fun (c,_) -> c=concrete_id) m) graph in
    (n',mg,g) in

  let sup = Pattern.merge_on_inf domain m2' cc1 cc2 in
  let (n1',mg1,g1) = find_cc id1 n1 m1 graph1 in
  let (n2',mg2,g2) = find_cc id2 n2 m2 graph2 in
  let m = compose_matchings mg1 m1 n1' m2' n2' m2 mg2 in
  (* let () = if (!Param.debug_mode) then print_build_context sigs m sup in *)
  match sup with
  | (None,_) -> ()
  | (Some s,_) ->
     let context = Pattern.merge_on_inf domain m g1 g2 in
     match context with
     | (None,conflict) ->
        (Format.printf "no context for inhibition due to conflict ";
         match conflict with
         | (i1,s1,_,int1)::(i2,s2,_,int2)::_ ->
            if (int1!= int2) then
              Format.printf "on internal state of %d:%d and %d:%d" i1 s1 i2 s2
            else Format.printf "on link of %d:%d and %d:%d" i1 s1 i2 s2
         | _ ->
            raise (ExceptDefn.Internal_Error("conflict must return two nodes")))
     | (Some c,_) ->
        (Format.printf "witness context for inhibition = ";
         Transition.print_cc sigs c)

let type_id_of_test f = function
  | Instantiation.Is_Here a | Instantiation.Has_Internal ((a,_),_)
    | Instantiation.Is_Free (a,_) | Instantiation.Is_Bound (a,_)
    | Instantiation.Has_Binding_type ((a,_),_) ->
     f (Matching.Agent.get_type a) (Matching.Agent.get_id a)
  | Instantiation.Is_Bound_to ((a,_),(a',_)) ->
     f (Matching.Agent.get_type a) (Matching.Agent.get_id a);
     f (Matching.Agent.get_type a') (Matching.Agent.get_id a')

let id_site_of_test id site test =
  let comp a s = ((Matching.Agent.get_id a)=id)&&(s=site) in
  match test with
  | Instantiation.Is_Here a -> false
  | Instantiation.Has_Internal ((a,s),_)
    | Instantiation.Is_Free (a,s) | Instantiation.Is_Bound (a,s)
    | Instantiation.Has_Binding_type ((a,s),_) -> comp a s
  | Instantiation.Is_Bound_to ((a1,s1),(a2,s2)) -> (comp a1 s1)||(comp a2 s2)

(* Has_Binding_type below is not ok, i would need to use Pattern.find_ty*)
let test_on_site tests (link,internal) =
  let (ok,rest) =
    if (internal = (-1)) then (true,tests)
    else
      let (int_test,rests) =
        List.partition
          (function
             Instantiation.Has_Internal ((_,_),i) -> (i=internal)
           | _ -> false) tests in
      if (int_test = []) then (false,[]) else (true,rests) in
  if (not ok) then false
  else
    let (ok',rest') =
      match link with
      | Pattern.UnSpec -> (true,rest)
      | Pattern.Free ->
         let (lnk_test,rest') =
           List.partition
             (function Instantiation.Is_Free _ -> true | _ -> false) rest in
         if (lnk_test = []) then (false,[]) else (true,rest')
       | Pattern.Link (nid,sid) ->
          let comp a s = ((Matching.Agent.get_id a)=nid)&&(s=sid) in
          let (lnk_test,rest') =
            List.partition
              (function
                 Instantiation.Is_Bound _
               | Instantiation.Has_Binding_type _ -> true
               | Instantiation.Is_Bound_to ((a1,s1),(a2,s2)) ->
                  (comp a1 s1)||(comp a2 s2)
               | _ -> false) rest in
          if (lnk_test = []) then (false,[]) else (true,rest') in
    (ok')&&(rest'=[])

let pairing_cc_event sigs tests cc =
(*  let () =
    Format.printf "in pairing_cc_event@.";
    Format.printf "cc = "; Transition.print_cc sigs cc;
    Format.printf "hh@. " in*)
  let rec pick ls = function
    | [] -> (-1,[])
    | hd::tl -> if (List.mem hd ls) then (hd,tl)
                else
                  let (i,rs) = pick ls tl in (i,hd::rs) in
  let f_agent ~pos ~agent_type acc =
    let possible =
      List.fold_left
        (fun acc test ->
          type_id_of_test
            (fun tty tid ->
              if ((agent_type = tty)&&(not(List.mem tid acc))) then tid::acc
              else acc)
            test) [] tests in
    (pos,possible)::acc in
  let f_site ~pos ~site state acc =
    let (_,possible_for_pos) = List.find (fun (p,l) -> p=pos) acc in
    let ok =
      List.fold_left
        (fun acc tid ->
          let pos_tests = List.filter (id_site_of_test tid site) tests in
          if (test_on_site pos_tests state) then tid::acc else acc)
        [] possible_for_pos in
    List.map (fun (p,l) -> if (p=pos) then (p,ok) else (p,l)) acc in
  let poss = Pattern.fold f_agent f_site cc [] in
  (* let () = *)
  (*   if (!Param.debug_mode) then *)
  (*     (Format.printf "@.poss = "; *)
  (*      List.iter (fun (p,l) -> Format.printf "(%d): " p; *)
  (*                              List.iter (Format.printf "%d,") l) poss) in *)
  let unused = List.fold_left (fun acc (p,l) -> l@acc) [] poss in
  let (pairs,_) =
    List.fold_left
      (fun (pairs,unused) (p,l) ->
        let (j,unused') = pick l unused in ((p,j)::pairs,unused'))
      ([],unused) poss in
  pairs

let matching_rule_abstract sigs ccs_rule abstract_event =
  List.fold_left2
    (fun acc tests (_,cc,id) ->
      let m = pairing_cc_event sigs tests cc in (m,id)::acc)
    [] abstract_event.Instantiation.tests ccs_rule

(*let s1 = past_of e1 s1 in*)
let context_of_application e1 s1 e2 s2 sigs env =
  let context s =
    let t = concret s sigs in
    let trans = TraceConcret.last_transition t in
    let graph = trans.Transition.lhs in
    let event = Poset.get_event_by_id (trans.Transition.eid) s in
    let rule = Model.get_rule env (Event.rule_id event) in
    let ccs_of_rule =
      Array.to_list
        (Array.map
           (fun id ->
             let cc =
               Pattern.Env.content (Pattern.Env.get (Model.domain env) id) in
             ([],cc,id))
           (Primitives.extract_cc_ids rule)) in
    let abstract_event = Primitives.extract_abstract_event rule in

    let step = event.Event.step in
    let tests = Trace.tests_of_step step in
    let (actions,side_effects_src) = Trace.actions_of_step step in
    let concrete_event =
      {Instantiation.tests=[tests]; actions;side_effects_src;
       side_effects_dst= []; connectivity_tests=[]} in
    let m =
      Instantiation.matching_abstract_concrete abstract_event concrete_event in
    let n = matching_rule_abstract sigs ccs_of_rule abstract_event in
    (* let () = if (!Param.debug_mode) then print_ac sigs tests actions in *)
    (graph,ccs_of_rule,m,n) in

  if (Poset.same_poset s1 s2) then ()
  else
    (let (graph1,lhs1,m1,n1) = context s1 in
     let (graph2,lhs2,m2,n2) = context s2 in

     match (graph1,graph2) with
     | (None,_) | (_,None) ->
        raise(ExceptDefn.Malformed_Args
                ("cannot evaluate negative influence on intro events"))
     | (Some graph1,Some graph2) ->
        (match (m1,m2) with
         | (None,_) | (_,None) ->
            raise(ExceptDefn.Internal_Error("rule instantiations"))
         | (Some m1,Some m2) ->
            let () = if (!Param.debug_mode) then
                       (print_context sigs graph1 lhs1 m1 n1 1;
                        print_context sigs graph2 lhs2 m2 n2 2) in
            let domain = Pattern.PreEnv.of_env (Model.domain env) in
            List.iter
              (fun ((_,cc1,_) as l1) ->
                List.iter
                  (fun ((_,cc2,_) as l2) ->
                    let infs = Pattern.infs cc1 cc2 in
                    List.iter
                      (fun inf ->
                        if (negative_influence inf) then
                          (List.iter
                             (fun m2' ->
                               let () = if (!Param.debug_mode)
                                        then print_infs sigs inf m2' l1 l2 in
                               build_context
                                 sigs domain l1 l2 graph1 graph2 m1 n1 m2 n2 m2')
                             (Pattern.matchings inf cc2))) infs) lhs2) lhs1))
