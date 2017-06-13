(******* printing functions for testing *)
let print_context sigs graph lhs m id =
  Format.printf "lhs%d = " id;Lib.print_side sigs lhs;
  Format.printf "graph%d = " id;Lib.print_side sigs graph;
  (* Format.printf "@.matching rule_cc_abstract= "; *)
  (* List.iter (fun (ls,_) -> *)
  (*             Format.printf "cc: ";Lib.print_int_list ls) n; *)
  Format.printf "@.matching abstract_concrete =@.";Lib.print_int_list m

let print_infs sigs inf m2' l1 l2 =
  Format.printf "@.l1 = ";Lib.print_side sigs [l1];
  Format.printf "@.l2 = ";Lib.print_side sigs [l2];
  Format.printf "@.inf @.";Lib.print_cc sigs inf;
  Format.printf "@.m2' = %a " Renaming.print_full m2'

let print_build_context sigs m sup =
  (match sup with
  | Some s -> Format.printf "sup = ";Lib.print_cc sigs s
  | None -> Format.printf "@.No sup ");
  Format.printf "@.m2'' = %a " Renaming.print_full m

let print_morphs1 rev_mg1 rev_m1 rev_n1 n2 =
  Format.printf "@.rev_mg1 = ";
  List.iter (fun (a,b)-> Format.printf "(%d,%d) " a b) rev_mg1;
  Format.printf "@.rev_m1 = ";
  List.iter (fun (a,b)-> Format.printf "(%d,%d) " a b) rev_m1;
  Format.printf "@.rev_n1 = ";
  List.iter (fun (a,b)-> Format.printf "(%d,%d) " a b) rev_n1;
  Format.printf "@.n2 = ";
  List.iter (fun (a,b)-> Format.printf "(%d,%d) " a b) n2

let print_morphs2 compose1 partial1 =
  Format.printf "@.compose1 = ";
  List.iter (fun (a,b)-> Format.printf "(%d,%d) " a b) compose1;
  Format.printf "@.partial1 = ";
  List.iter (fun (a,b)-> Format.printf "(%d,%d) " a b) partial1

let print_morphisms compose2 =
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

let compose_total m1 m2 =
  List.fold_left
    (fun acc (a1,b1) ->
      try
        let (_,b2) = List.find (fun (a,_) -> (b1=a)) m2 in
        (a1,b2)::acc
      with Not_found -> acc) [] m1

let reverse m = List.map (fun (a,b) -> (b,a)) m

(******************************************************************)

let inf_modified_by_actions inf actions n1 m1 =
  let modified a s (link,internal) action =
    let comp a' s' = ((Agent.id a')=a)&&(s'=s) in
    match action with
    | Instantiation.Create _ -> false
    | Instantiation.Mod_internal ((a',s'),i) ->
       if (comp a' s') then (not(internal= -1)) else false
    | Instantiation.Bind ((a1,s1),(a2,s2))
      | Instantiation.Bind_to ((a1,s1),(a2,s2)) ->
       if ((comp a1 s1)||(comp a2 s2))
       then (not(link = Pattern.UnSpec)) else false
    | Instantiation.Free (a',s') ->
       if (comp a' s') then (not(link = Pattern.UnSpec)) else false
    | Instantiation.Remove a' -> ((Agent.id a')=a) in
  let f_agent ~pos ~agent_type acc = acc in
  let f_site ~pos ~site state acc =
    let (_,abstract_id) = List.find (fun (i,_) -> i=pos) n1 in
    let (_,concrete_id) = List.find (fun (i,_) -> i=abstract_id) m1 in
    List.fold_left
      (fun acc action ->
        acc||(modified concrete_id site state action)) acc actions in
  Pattern.fold f_agent f_site inf false

let negative_influence domain inf m2' cc1 cc2 actions n1 m1 =
  if (inf_modified_by_actions inf actions n1 m1) then
    let sup = Pattern.merge_on_inf domain m2' cc1 cc2 in
    match sup with
    | (None,_) -> false
    | (Some _,_) -> true
  else false

let compose_matchings mg1 m1 n1 m2' n2 m2 mg2 =
  let reverse_and_filter n m =
    reverse (List.filter (fun (a,b) -> (List.mem b (image n))) m) in
  let rev_mg1 = reverse mg1 in
  let rev_m1 = reverse_and_filter rev_mg1 m1 in
  let rev_n1 = reverse_and_filter rev_m1 n1 in
  let () = if (!Param.debug_mode) then print_morphs1 rev_mg1 rev_m1 rev_n1 n2 in
  let aux = compose_total rev_mg1 rev_m1 in
  (*let () = Format.printf "rev_mg1+rev_m1 = ";
           List.iter (fun (a,b) -> Format.printf "(%d,%d) " a b) aux in*)
  let compose1 = compose aux rev_n1 in
  let list_m2' = Renaming.to_list m2' in
  (* partial1: g1 -> inf *)
  let partial1 =
    List.filter (fun (a1,b1) -> List.mem b1 (domain list_m2')) compose1 in
  let () = if (!Param.debug_mode) then print_morphs2 compose1 partial1 in
(*  let comp1 = compose partial1 list_m2' in
  let () = print_morphisms comp1 in
  let comp2 = compose comp1 n2 in
  let () = print_morphisms comp2 in
  let comp3 = compose comp2 m2 in
  let () = print_morphisms comp3 in
  let () = print_morphisms mg2 in *)
  let compose2 =
    compose (compose (compose (compose partial1 list_m2') n2) m2) mg2 in
  let () = if (!Param.debug_mode) then print_morphisms compose2 in
  list_to_renaming compose2

let print_conflict sigs (cc1,i1,cc2,i2,s,internal) =
  let () =
    Format.printf "no context for inhibition due to conflict on cc1 = ";
    Lib.print_cc sigs cc1;
    Format.printf " and cc2 = "; Lib.print_cc sigs cc2;
    Format.printf " due to conflict on " in
  let () =
    if internal then Format.printf "internal states of "
    else Format.printf "links of " in
  Format.printf "%d of cc1 and %d of cc2 on site %d @." i1 i2 s

let build_context
      sigs domain (n1,cc1,id1) (n2,cc2,id2) graph1 graph2 m1 m2 m2' inf=
  let find_cc n m graph =
    (*graph:(morphism,pattern.cc,pattern.id) list*)
    let (rule_id,abstract_id) = List.hd n in
    let (_,concrete_id) = List.find (fun (i,_) -> (i=abstract_id)) m in
    let cc_of_graph,rest =
      List.partition
        (fun (m,_,_) -> List.exists (fun (c,_) -> c=concrete_id) m) graph in
    match cc_of_graph with
    | (mg,g,_)::[] -> (n,mg,g,rest)
    | _ ->
       raise (ExceptDefn.Internal_Error("only one cc should match in graph")) in
  let (n1',mg1,g1,rest1) = find_cc n1 m1 graph1 in
  let (n2',mg2,g2,rest2) = find_cc n2 m2 graph2 in
  (*let () =
    Format.printf "build_context @.@.";
    Lib.print_cc sigs g1;Lib.print_cc sigs g2;
    Lib.print_cc sigs cc1;Lib.print_cc sigs cc2 in*)
  let m = compose_matchings mg1 m1 n1' m2' n2' m2 mg2 in
  (* let () = if (!Param.debug_mode) then print_build_context sigs m in *)
  let context = Pattern.merge_on_inf domain m g1 g2 in
  match context with
  | (None,Some conflict) ->
     let () = if (!Param.verb) then print_conflict sigs conflict in
     false
  | (None,None) ->
     raise (ExceptDefn.Internal_Error("no pushout must return conflict"))
  | (Some c,_) ->
     let () =
       if (!Param.verb) then
         (Format.printf "witness context for inhibition = ";
          Lib.print_cc sigs c;
          List.iter
            (fun (_,cc,_) ->
              Pattern.print_cc
                ~new_syntax:true ~sigs ~with_id:true (Format.std_formatter) cc)
            rest1;
          List.iter
            (fun (_,cc,_) ->
              Pattern.print_cc
                ~new_syntax:true ~sigs ~with_id:true (Format.std_formatter) cc)
            rest2;
          Format.printf "@. due to inf = ";
          Lib.print_cc sigs inf; Format.printf "@.") in
     true

let all_possible lhs1 lhs2 sigs domain graph1 graph2 m1 m2 actions =
  let infl = ref false in
  let res =
    List.fold_left
     (fun ok1 ((n1,cc1,_) as l1) ->
      let ok1' =
        List.fold_left
          (fun ok2 ((_,cc2,_) as l2) ->
            let infs = Pattern.infs cc1 cc2 in
            let ok2' =
              List.fold_left
                (fun ok3 inf ->
                  let ok3'=
                    List.fold_left
                      (fun ok m2' ->
                        let () =
                          if (!Param.debug_mode)
                          then print_infs sigs inf m2' l1 l2 in
                        let infl' =
                          negative_influence
                            domain inf m2' cc1 cc2 actions n1 m1 in
                        let () = infl := infl'|| (!infl) in
                        if (not infl') then false
                        else
                          ok||
                            (build_context
                               sigs domain l1 l2 graph1
                               graph2 m1 m2 m2' inf))
                      false (Pattern.matchings inf cc2)
                  in ok3||ok3') false infs
            in ok2||ok2') false lhs2
      in ok1||ok1') false lhs1 in
  if !(infl) then res
  else
    let () =
      if (!Param.verb) then
        Format.printf "no inhibition due to no negative influence on rules " in
    res

let context_of_application e1 s1 e2 s2 sigs env =
  let env' = Model.domain env in
  let domain = Pattern.PreEnv.of_env env' in
  let (graph1,lhs1,m1,actions) = Poset.concrete env env' sigs e1 s1 in
  let (graph2,lhs2,m2,_) = Poset.concrete env env' sigs e2 s2 in

  let () = if (!Param.debug_mode) then
             (print_context sigs graph1 lhs1 m1 1;
              print_context sigs graph2 lhs2 m2 2) in
  all_possible lhs1 lhs2 sigs domain graph1 graph2 m1 m2 actions

let inhibition e1 s1 e2 s2 sigs env =
  (*if (Poset.same_poset s1 s2) then false
  else*)
    context_of_application e1 s1 e2 s2 sigs env
