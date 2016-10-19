open Yojson
open Lib

type t = {
    kappa : bool;
    filename : string option;
    events : Event.t list;
    prec_1 : (int * int) list;
    inhibit : (int * int) list;
  }

type enriched = {
    pos : t;
    prec_star : int list array;
  }

let empty_poset =
  {kappa = false; filename = None; events = []; prec_1 = []; inhibit = [];}

let get_events_from_poset p = p.events

let get_event_by_id i p =
  List.find (fun e -> (Event.get_id e) = i) p.events

let get_events_by_id_list ls p =
  List.filter (fun e -> List.mem (Event.get_id e) ls) p.events

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

let read_poset_from_file file =
  let json = Yojson.Basic.from_file file in
  let open Yojson.Basic.Util in
  let nodes = json |> member "nodes" |> to_list in
  let edges_cause = json |> member "cause" |> to_list in
  let edges_inhibit = json |> member "inhibit" |> to_list in
  { kappa = true;
    filename = Some file;
    events = List.mapi (fun i l -> Event.nodes_of_json l) nodes;
    prec_1 = edges_of_json edges_cause;
    inhibit = edges_of_json edges_inhibit}

let test_poset =
  let e0 = Event.test_event 1 "A" in
  let e1 = Event.test_event 2 "B" in
  { kappa = false;filename = None;events = [e0;e1];prec_1 = [];inhibit =[] }

let split_intro_rest p =
  List.fold_left
    (fun (intros,rest) (e1,e2) ->
      if (List.mem e1 rest) then
        ( if (List.mem e2 rest) then (intros,rest) else (intros,e2::rest))
      else (e1::intros,e2::rest))
    ([],[]) p.prec_1

let get_poset_of_events events =
  { kappa = false; filename = None; events; prec_1 = []; inhibit = [] }

let intro p =
  let intros =
    List.filter
    (fun e ->
      not(List.exists (fun (i1,i2) -> (Event.get_id e) = i2) p.prec_1))
    p.events in
  let () = if (!Parameter.debug_mode)
           then (Format.printf "intro events of poset: \n";
                 print_poset p;
                 Format.printf "are: \n";
                 List.iter (fun e -> Event.print_event e) intros) in
  get_poset_of_events(intros)

(* specialised to the case of one obs - the last one in the list *)
let remove_obs p =
  if (p.kappa) then
    (let obs_id = List.length p.events in
     let events =
       List.filter (fun e -> not ((Event.get_id e) = obs_id)) p.events in
     let prec_1 = List.filter (fun (e1,e2) -> not (e2 = obs_id) ) p.prec_1 in
     let inhibit = List.filter (fun (e1,e2) -> not (e2 = obs_id) ) p.inhibit in
     { kappa = false; filename = p.filename; events; prec_1; inhibit; })
  else raise (ExceptionDefn.Internal_Error("should not be possible"))

let sort_prec ls = ls

(* id of events in interval [0,length(events)] *)
let get_enriched p =
  let arr = Array.make (List.length p.events) [] in
  let sorted = sort_prec p.prec_1 in
  let prec = List.rev sorted in
  let () =
    List.iter
      (fun (e1,e2) ->
        let l2 =
          List.fold_left
            (fun acc e -> if (List.mem e acc) then acc else e::acc)
            arr.(e1) arr.(e2) in
        arr.(e1) <- e2::l2) prec in
  { pos = p; prec_star = arr }

let check_prec_1 e1 e2 p =
  List.mem (e1.Event.event_id, e2.Event.event_id) p.prec_1

let check_prec_star e1 e2 p =
  let enrich = get_enriched p in
  let arr = enrich.prec_star in
  List.mem (e2.Event.event_id) arr.(e1.Event.event_id)
