open Yojson
open Lib

type t = {
    kappa : bool;
    filename : string option;
    events : Event.t list;
    prec_1 : (int * int) list;
    inhibit : (int * int) list;
    mutable prec_star : (int*int list) list option;
  }

let empty_poset =
  {kappa = false; filename = None; events = []; prec_1 = [];
   prec_star = None; inhibit = []}

let events p = p.events
let filename p = p.filename

let event_by_id i p =
  List.find (fun e -> (Event.id e) = i) (p.events)

let events_by_id_list ls p =
  List.filter (fun e -> List.mem (Event.id e) ls) p.events

let print_poset_name p = match p.filename with
  | Some fn -> Format.printf "%s" fn
  | None -> Format.printf "(no name)"

let print_poset p =
  Format.printf "@.";
  print_poset_name p;
  Format.printf " with events (id, label):";
  List.iter (fun e -> Event.print_event e) p.events;
  Format.printf "@.";
  Format.printf "prec : ";
  List.iter (fun (e,e') -> Format.printf " %i < %i  " e e') p.prec_1 ;
  Format.printf "@.";
  Format.printf "inhibit : ";
  List.iter (fun (e,e') -> Format.printf " %i < %i " e e') p.inhibit;
  Format.printf "@."

let edges_of_json json =
  let open Yojson.Basic.Util in
  List.map
    (fun json ->
      let e1 = member "from" json |> to_int in
      let e2 = member "to" json |> to_int in
      (e1,e2)) json

let read_poset_from_file file env =
  let json = Yojson.Basic.from_file file in
  let open Yojson.Basic.Util in
  let nodes = json |> member "nodes" |> to_list in
  let edges_cause = json |> member "cause" |> to_list in
  let edges_inhibit = json |> member "inhibit" |> to_list in
  { kappa = true;
    filename = Some file;
    events = List.mapi (fun i l -> Event.nodes_of_json env l) nodes;
    prec_1 = edges_of_json edges_cause;
    prec_star = None;
    inhibit = edges_of_json edges_inhibit}

let get_poset_of_events events =
  { kappa = false; filename = None; events; prec_1 = [];
    prec_star=None; inhibit = []}

let intro p =
  let intro_events =
    List.filter
      (fun e -> not(List.exists (fun (_,i) -> (Event.id e) = i) p.prec_1))
      p.events in
  get_poset_of_events(intro_events)

let obs p =
  let obs_events =
    List.filter
      (fun e -> not(List.exists (fun (i,_) -> (Event.id e) = i) p.prec_1))
      p.events in
  get_poset_of_events(obs_events)

(* mophisms between posets don't take obs into account - only one obs in kappa *)
let prepare_for_morphism p =
  if (not(p.kappa)) then p
  else
    let obs_id = List.hd (List.map (fun e -> (Event.id e)) (events (obs p))) in
    let neither (e1,e2) = not((e1 = obs_id)||(e2 = obs_id)) in
    let events = List.filter (fun e -> not (Event.id e = obs_id)) p.events in
    let prec_1 = List.filter neither p.prec_1 in
    let inhibit = List.filter neither p.inhibit in
    {kappa = false;filename = p.filename;events;prec_1;prec_star=None;inhibit;}

let sort p =
  (*let rev_inhibit = List.map (fun (a,b) -> (b,a)) p.inhibit in*)
  let to_order = p.prec_1 @ p.inhibit in
  let rec sort_pairs = function
    | [] -> []
    | ls ->
       let (max,rest) =
         List.partition
           (fun (a,b) -> not (List.exists (fun (_,a') -> (a = a')) ls)) ls in
       max@(sort_pairs rest) in
  sort_pairs to_order

let obs_id p =
  let obs_pos = obs p in
  List.map (Event.id) obs_pos.events

let linearisation p =
  let l = Lib.remove_duplicates (List.map fst (sort p)) (=) in
  l@(obs_id p)

let get_enriched p =
  match p.prec_star with
  | Some enr -> enr
  | None ->
     let sorted = sort p in
     let () =
       if (!Param.debug_mode) then
         (Format.printf "sort poset @."; Lib.print_int_list sorted) in

     let get_causes e acc =
       let direct_causes =
         List.map fst (List.filter (fun (_,e') -> e' = e) sorted) in
       List.fold_left
         (fun acc' e' ->
           let (_,causes) = List.find (fun (i,_) -> i=e') acc in
           Lib.concat_without_duplicates causes acc') [e] direct_causes in

     let indirect ls acc' =
       List.fold_left
         (fun acc e1 ->
           if (List.exists (fun (e,_) -> e = e1) acc) then acc
           else (e1,get_causes e1 acc)::acc) acc' ls in
     let s = indirect (List.map fst sorted) [] in
     let obs =
       List.find_all
         (fun e -> not(List.exists (fun (e',_) -> e=e') s))
         (List.map Event.id p.events) in
     let s' = indirect obs s in
     let () =
       if (!Param.debug_mode) then
         (Format.printf "enriched poset @.";
          List.iter
            (fun (a,ls) ->
              Format.printf "[%d]:" a;List.iter (Format.printf "%d, ") ls;
              Format.printf "@.") s') in
     let () = p.prec_star <- Some s' in
     s'

let check_prec_1 e1 e2 p =
  (List.mem e1 p.events)&& (List.mem e2 p.events)&&
    (List.mem (e1.Event.event_id, e2.Event.event_id) p.prec_1)

let check_prec_star e1 e2 p =
  let enrich = get_enriched p in
  if (List.mem e1 p.events)&&(List.mem e2 p.events) then
    let (_,causes_e2) = List.find (fun (e,_) -> e=(e2.Event.event_id)) enrich in
    (List.mem (e2.Event.event_id) causes_e2)
  else false

let past e p =
  let enrich = get_enriched p in
  let eid = Event.id e in
  let (_,causes) = List.find (fun (i,_) -> i = eid) enrich in
  let events = events_by_id_list causes p in
  let list_mem (e1,e2) =
    (List.mem e1 causes)&&(List.mem e2 causes) in
  let prec_1 = List.filter list_mem p.prec_1 in
  let inhibit = List.filter list_mem p.inhibit in
  {kappa = false;filename = p.filename;events;prec_1;prec_star=None;inhibit;}

let same_poset p1 p2 =
  match (p1.filename,p2.filename) with
  | (None, _) | (_, None) -> false
  | (Some f1, Some f2) -> ((String.compare f1 f2)=0)


(** concretisation to trace ****)

let print_replay_states event lhs_state rhs_state =
  Format.printf "@.concret of event";Event.print_event event;
  Format.printf "lhs_state@.";
  Edges.debug_print (Format.std_formatter)
                    (lhs_state.Replay.graph);
  Format.printf "rhs_state@.";
  Edges.debug_print (Format.std_formatter)
                    (rhs_state.Replay.graph)

let copy_state s = {
    Replay.graph = (Edges.copy s.Replay.graph); time=s.Replay.time;
    event = s.Replay.event;connected_components=s.Replay.connected_components;}

let make sigs env replay_state =
  let (env',list) = Replay.cc_of_state replay_state env in
  (env',list)

let concretise env s linear sigs =
  let () = if (!Param.debug_mode) then
             (Format.printf "concretise linear poset :";
              List.iter (fun i -> Format.printf "%d " i) linear) in
  let init_state = Replay.init_state ~with_connected_components:true in
  let (trace,_,_) =
    List.fold_left
      (fun (trace,lhs_state,env) eid ->
        let event = event_by_id eid s in
        let step = Event.step event in
        let rhs_copy = copy_state lhs_state in
        let (rhs_state,_) = Replay.do_step sigs rhs_copy step in
        (* let () = if (!Param.debug_mode) then *)
        (*            print_replay_states event lhs_state rhs_state in *)
        let (env',graph) = make sigs env rhs_state in
        let trace' = TraceConcret.add_transition sigs trace graph eid in
        (trace',rhs_state,env'))
      (TraceConcret.empty,init_state,(Pattern.PreEnv.of_env env))
      linear in
  (List.rev trace)

let concrete model env sigs e p =
  match (Event.concrete e) with
    None ->
    let past_e = past e p in
    let () =
      if (!Param.debug_mode) then
        (Format.printf "past of e@.";print_poset past_e) in
    let linear = linearisation past_e in
    let t = concretise env past_e linear sigs in
    let lhs_graph = TraceConcret.get_last_lhs_graph t in
    let graph = match lhs_graph with
      | None ->
         raise(ExceptDefn.Malformed_Args
                 ("cannot evaluate negative influence on intro events"))
      | Some graph -> graph in
    Event.set_concrete model env sigs e graph
  | Some context -> context
