open Yojson
open Lib

type t = {
    kappa : bool;
    filename : string option;
    events : Event.t list;
    prec_1 : (int * int) list;
    inhibit : (int * int) list;
    mutable prec_star : int list array option;
  }

let empty_poset =
  {kappa = false; filename = None; events = []; prec_1 = [];
   prec_star = None; inhibit = [];}

let get_events_from_poset p = p.events

let get_event_by_id i p =
  List.find (fun e -> (Event.id e) = i) (get_events_from_poset p)

let get_events_by_id_list ls p =
  List.filter (fun e -> List.mem (Event.id e) ls) p.events

let print_poset p =
  Format.printf "events (id, label) : \n";
  List.iter (fun e -> Event.print_event e) p.events;
  Format.printf "\n";
  Format.printf "prec : ";
  List.iter (fun (e,e') -> Format.printf " %i < %i  " e e') p.prec_1 ;
  Format.printf "\n";
  Format.printf "inhibit : ";
  List.iter (fun (e,e') -> Format.printf " %i < %i " e e') p.inhibit;
  Format.printf "\n"

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

let split_intro_rest p =
  List.fold_left
    (fun (intros,rest) (e1,e2) ->
      if (List.mem e1 rest) then
        ( if (List.mem e2 rest) then (intros,rest) else (intros,e2::rest))
      else (e1::intros,e2::rest))
    ([],[]) p.prec_1

let get_poset_of_events events =
  { kappa = false; filename = None; events; prec_1 = [];
    prec_star=None; inhibit = [] }

let intro p =
  let intros =
    List.filter
    (fun e ->
      not(List.exists (fun (i1,i2) -> (Event.id e) = i2) p.prec_1))
    p.events in
  let () = if (!Param.debug_mode)
           then (Format.printf "intro events of poset: \n";
                 print_poset p;
                 Format.printf "are: \n";
                 List.iter (fun e -> Event.print_event e) intros) in
  get_poset_of_events(intros)

(* specialised to the case of one obs - the last one in the list *)
let obs p =
  if (p.kappa) then List.length p.events
  else raise (ExceptDefn.Internal_Error("obs of non kappa poset"))

let remove_event p eid =
  let neither (e1,e2) = not((e1 = eid)||(e2 = eid)) in
  let events = List.filter (fun e -> not ((Event.id e) = eid)) p.events in
  let prec_1 = List.filter neither p.prec_1 in
  let inhibit = List.filter neither p.inhibit in
  {kappa = false;filename = p.filename; events; prec_1;prec_star= None;inhibit}

let sort_prec ls =
  let rec verif_sort = function
    | (e1,e2)::next ->
       if not(List.exists (fun (e1',e2') -> (e2 = e1')) next)
       then verif_sort next else false
    | [] -> true in
  if (verif_sort ls) then ls
  else raise (ExceptDefn.Internal_Error("prec_1 is not sorted"))

(* id of events in interval [0,length(events)] *)
let get_enriched p =
  match p.prec_star with
  | Some enr -> enr
  | None ->
     let arr = Array.make ((List.length p.events)+1) [] in
     let sorted = sort_prec p.prec_1 in
     let () =
       List.iter
         (fun (e1,e2) ->
           let l2 =
             List.fold_left
               (fun acc e -> if (List.mem e acc) then acc else e::acc)
               arr.(e1) arr.(e2) in
           arr.(e1) <- e2::l2) sorted in
     let () = p.prec_star <- Some arr in
     arr

let check_prec_1 e1 e2 p =
  (List.mem e1 p.events)&& (List.mem e2 p.events)&&
    (List.mem (e1.Event.event_id, e2.Event.event_id) p.prec_1)

let check_prec_star e1 e2 p =
  let enrich = get_enriched p in
  let () =
    if (!Param.debug_mode) then
      (Format.printf "check_prec_star \n";
       Array.iteri
         (fun i l -> Format.printf "arr(%d) =" i;
                     List.iter (fun id -> Format.printf "%d " id) l;
                     Format.printf "\n") enrich;
       Format.printf "%d =< %d" (e1.Event.event_id) (e2.Event.event_id) ) in
  (List.mem e1 p.events)&& (List.mem e2 p.events)&&
    (List.mem (e2.Event.event_id) enrich.(e1.Event.event_id))

(*check it !!*)
let past e p =
  let enrich = get_enriched p in
  let eid = Event.id e in
  let events = get_events_by_id_list enrich.(eid) p in
  let list_mem (e1,e2) =
    (List.mem e1 enrich.(eid))&&(List.mem e1 enrich.(eid)) in
  let prec_1 = List.filter list_mem p.prec_1 in
  let inhibit = List.filter list_mem p.inhibit in
  {kappa = false;filename = p.filename; events; prec_1;prec_star= None;inhibit}

let same_poset p1 p2 =
  match (p1.filename,p2.filename) with
  | (None, _) | (_, None) -> false
  | (Some f1, Some f2) -> ((String.compare f1 f2)=0)
