open Yojson
open Lib

type c =
  (((int*int) list * Pattern.cc * Pattern.id) list * (* the context *)
     ((int*int) list * Pattern.cc * Pattern.id) list * (* the lhs of the rule *)
     (int * int) list * (* the morphism abstract_event -> concrete_event *)
       (*((int * int) list* Pattern.id) list * *)
         (* the morphisms ccs_rule -> abstract_event *)
         (Instantiation.concrete Instantiation.action list))
         (*the actions - used to detect negative influence btw rules*)
type t = {
    event_id : int; (* local id inside a story *)
    rule_id : int; (* id of the primitive rule *)
    event_label : string; (*label of corresponding ast rule *)
    step : Trace.step;
    mutable concrete : c option
  }

let id e = e.event_id
let rule_id e = e.rule_id
let label e = e.event_label
let step e = e.step
let concrete e = e.concrete

let print_event e =
  Format.printf "\n(%i, %s) " (e.event_id) (e.event_label)

let print_complete_event e =
  print_event e;Trace.print_step (Format.std_formatter) e.step;Format.printf"\n"

let rule_id_of_json = function
    | Trace.Rule (id,_,_) -> id
    | Trace.Pert _ -> -1
    | Trace.Init _ -> -2
    | Trace.Obs _ -> -3
    | Trace.Dummy _ | Trace.Subs _ -> -4

let nodes_of_json env = function
  | `List [`Int id; step_json] ->
     let step = Trace.step_of_yojson step_json in
     let () = Trace.print_label_of_step ?env (Format.str_formatter) step in
     let event_label = Format.flush_str_formatter () in
     let rule_id = rule_id_of_json step in
     { event_id = id; rule_id; event_label; step; concrete = None}
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect node of json",x))

(** concretisation functions ***)

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

let print_test_on_site a s link internal =
  Format.printf "test_on_site %d %d link = " a s;
  (match link with
   | Pattern.UnSpec -> Format.printf "Unspec"
   | Pattern.Free -> Format.printf "free"
   | _ -> Format.printf "bound");
  Format.printf " and intern = %d @." internal

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
let test_on_site a s tests (link,internal) =
  (*let () = if (!Param.debug_mode) then print_test_on_site a s link internal in*)
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
      | Pattern.Free ->
         let (lnk_test,rest') =
           List.partition
             (function Instantiation.Is_Free _ -> true | _ -> false) rest in
         if (lnk_test = []) then (false,[]) else (true,rest')
      | Pattern.UnSpec ->
          let (lnk_test,rest') =
           List.partition
             (function Instantiation.Is_Free _ -> true | _ -> false) rest in
          (true,rest') (**!! weird behavior of cc **)
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
  (* let () = *)
  (*   Format.printf "in pairing_cc_event@."; *)
  (*   Format.printf "cc = "; Lib.print_cc sigs cc; *)
  (*   Format.printf "@. " in *)
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
    (* let () = Format.printf "f_agent on %d with possible = " pos; *)
    (*          List.iter (Format.printf "%d ") possible; Format.printf "@." in *)
    (pos,possible)::acc in
  let f_site ~pos ~site state acc =
    let (_,possible_for_pos) = List.find (fun (p,l) -> p=pos) acc in
    let ok =
      List.fold_left
        (fun acc tid ->
          let pos_tests = List.filter (id_site_of_test tid site) tests in
          if (test_on_site pos site pos_tests state) then tid::acc else acc)
        [] possible_for_pos in
    List.map (fun (p,l) -> if (p=pos) then (p,ok) else (p,l)) acc in
  (* let () = Format.printf "pattern fold @." in *)
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
  let unmatched = List.exists (fun (a,b) ->  b = -1) in
  let (morphisms,unmatched_tests) =
    List.fold_left
      (fun (acc,available_tests) (_,cc,id) ->
        let (acc',remaining_tests,found) =
          List.fold_left
            (fun (acc,acc_tests,found) tests ->
              if found then (acc,tests::acc_tests,true)
              else
                let m = pairing_cc_event sigs tests cc in
                if not(unmatched m) then ((m,id)::acc,acc_tests,true)
                else (acc,tests::acc_tests,false))
            (acc,[],false) available_tests in
        if found then (acc',remaining_tests)
        else
          raise(ExceptDefn.Internal_Error
                  ("matching_rule_abstract: no test for pattern@.")))
      ([],abstract_event.Instantiation.tests) ccs_rule in
  if (unmatched_tests = []) then morphisms
  else
    raise(ExceptDefn.Internal_Error
            ("matching_rule_abstract: no pattern for tests@."))
(*  List.fold_left2
    (fun acc tests (_,cc,id) ->
      let m = pairing_cc_event sigs tests cc in (m,id)::acc)
    [] abstract_event.Instantiation.tests ccs_rule*)

let context_and_rule env env' sigs e =
  let rule = Model.get_rule env e.rule_id in
  let ccs_of_rule =
    Array.to_list
      (Array.map
         (fun id ->
           let cc =
             Pattern.Env.content (Pattern.Env.get env' id) in
           ([],cc,id))
         (Primitives.extract_cc_ids rule)) in
  let abstract_event = Primitives.extract_abstract_event rule in

  let tests = Trace.tests_of_step e.step in
  let (actions,side_effects_src) = Trace.actions_of_step e.step in
  let concrete_event =
    {Instantiation.tests=[tests]; actions;side_effects_src;
     side_effects_dst= []; connectivity_tests=[]} in
  let m =
    match (Instantiation.matching_abstract_concrete
             abstract_event concrete_event) with
    | None -> raise(ExceptDefn.Internal_Error("rule instantiations"))
    | Some m -> Renaming.to_list m in
  let n = matching_rule_abstract sigs ccs_of_rule abstract_event in
  let ccs_of_rule' =
    List.map
      (fun (_,cc,id) ->
        let (morphism,_) = List.find (fun (n',id') -> id = id') n in
        (morphism,cc,id)) ccs_of_rule in
  (* let () = if (!Param.debug_mode) then print_ac sigs tests actions in *)
  (ccs_of_rule',m,actions)

let set_concrete env env' sigs e graph =
  let (ccs_of_rule,m,actions) = context_and_rule env env' sigs e in
  let () = e.concrete <- Some (graph,ccs_of_rule,m,actions) in
  (graph,ccs_of_rule,m,actions)
