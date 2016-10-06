open Yojson
open Lib

module StringMap = Map.Make(String)

exception MalformedDatatype of string
exception ErrorParsing of string

type quark =
  | Tested of (int*int*int)
  | Modified of (int*int*int)
  | TestedMod of (int*int*int)

type event = {
    event_id : int; (* local id inside a story *)
    event_label : string;
    quarks : quark list;
  }

type poset = {
    events : event list;
    prec_1 : (int * int) list;
    inhibit : (int * int) list;
  }

type domain =  Pos of poset
             | Ev of event

type t = {
    poset_list : poset list;
    events_list : event list;
  }

let empty_poset =
  {events = []; prec_1 = []; inhibit = [];}

let get_events t = t.events_list

let get_posets t = t.poset_list

let get_events_from_poset p = p.events

let get_event_by_id i p =
  List.find (fun e -> e.event_id = i) p.events

let get_event_id e = e.event_id

let print_event e =
  Format.printf " (%i, %s)  " (e.event_id) (e.event_label)

let print_poset p =
  Format.printf "events (id, label) : \n";
  List.iter (fun e -> print_event e) p.events;
  Format.printf "\n";
  Format.printf "prec : ";
  List.iter (fun (e,e') -> Format.printf " %i < %i  " e e') p.prec_1 ;
  Format.printf "\n";
  Format.printf "inhibit : ";
  List.iter (fun (e,e') -> Format.printf " %i < %i " e e') p.inhibit;
  Format.printf "\n"

let print_posets t =
  List.iteri
    (fun i p -> Format.printf "\n poset nb %d\n" i; print_poset p)
    t.poset_list

let print_domain_list l =
  List.iter
    (fun d -> match d with
              | Pos p -> print_poset p
              | Ev e -> print_event e) l

let quarks_of_json (quarks:Yojson.Basic.json) =
  let open Yojson.Basic.Util in
  match quarks with
  | `List [`String impact; (`Assoc ["node", `Int node]);
           (`Assoc ["site", `Int site]); `String state] ->
     let state_int = match state with
       | "link" -> 1
       | "internal state" -> 0
       | x -> raise (Yojson.Basic.Util.Type_error
                       ("Not in the cflow format for link/internal state ",
                        `String x))
     in
     (match impact with
     | "tested" -> Tested (node,site,state_int)
     | "modified" -> Modified (node,site,state_int)
     | "tested + modified" -> TestedMod (node,site,state_int)
     | x -> raise (Yojson.Basic.Util.Type_error
                     ("Not in the cflow format for quarks",`String x)))
  | _ -> raise (Yojson.Basic.Util.Type_error
                  ("Not in the cflow format for quarks",`Null))

let nodes_of_json (node:Yojson.Basic.json) =
  let open Yojson.Basic.Util in
  match node with
  | `List [`Int id; `String "RULE"; `String label;
           (`Assoc ["quarks", `List l]) ]
    | `List [`Int id; `String "OBS"; `String label;
             (`Assoc ["quarks", `List l])]
    | `List [`Int id; `String "PERT"; `String label;
             (`Assoc ["quarks", `List l])]->
     let quarks_ls =
       List.map (fun q -> quarks_of_json q) l in
     { event_id = id; event_label = label; quarks = quarks_ls; }
  | `List [`Int id; `String "INIT"; `List l;
           (`Assoc ["quarks", `List ql])] ->
     let init_label =
       List.fold_left
         (fun lbl n ->
           match n with
             | `String i -> lbl^i
             | x -> raise (Yojson.Basic.Util.Type_error
                             ("Not in the cflow format",x))) "" l in
     let quarks_ls =
       List.map (fun q -> quarks_of_json q) ql in
     { event_id = id; event_label = init_label; quarks = quarks_ls; }
  | _ -> raise (Yojson.Basic.Util.Type_error ("Not in the cflow format",`Null))

let edges_of_json json =
  let open Yojson.Basic.Util in
  List.map
    (fun json ->
      let e1 = member "from" json |> to_int in
      let e2 = member "to" json |> to_int in
      (e1,e2)) json

let read_posets_from_files current_stories file =
  let json = Yojson.Basic.from_file file in
  let open Yojson.Basic.Util in
  let nodes = json |> member "nodes" |> to_list in
  let edges_cause = json |> member "cause" |> to_list in
  let edges_inhibit = json |> member "inhibit" |> to_list in
  let s : poset =
    { events = List.mapi (fun i l -> nodes_of_json l) nodes;
      prec_1 = edges_of_json edges_cause;
      inhibit = edges_of_json edges_inhibit} in
  { poset_list = s::current_stories.poset_list;
    events_list = s.events@current_stories.events_list; }

let test_poset current_stories =
  let e0 : event = { event_id = 1;
                     event_label = "A"; quarks = []} in
  let e1 : event = { event_id = 2;
                     event_label = "B"; quarks = []} in
  let s : poset =
    { events = [e0;e1];
      prec_1 = [];
      inhibit =[] } in
  { poset_list = s::current_stories.poset_list;
    events_list = [e0;e1]@current_stories.events_list; }

let set_posets file_list =
  let posets : t = { poset_list = []; events_list = []; } in
  let posets_file =
    List.fold_left
      (fun t file ->
        read_posets_from_files t file) posets file_list in
  let posets' = test_poset posets_file in
  let () = if (!Parameter.debug_mode)
           then (Format.printf "set_posets: \n";
                 print_posets posets') in
  posets'

let split_intro_rest p =
  List.fold_left
    (fun (intros,rest) (e1,e2) ->
      if (List.mem e1 rest) then
        ( if (List.mem e2 rest) then (intros,rest) else (intros,e2::rest))
      else (e1::intros,e2::rest))
    ([],[]) p.prec_1

let get_poset_of_events events =
  { events; prec_1 = []; inhibit = []}

let intro p =
  let intros =
    List.filter
    (fun e ->
      not(List.exists (fun (i1,i2) -> e.event_id = i2) p.prec_1))
    p.events in
  let () = if (!Parameter.debug_mode)
           then (Format.printf "intro events of poset: \n";
                 print_poset p;
                 Format.printf "are: \n";
                 List.iter (fun e -> print_event e) intros) in
  get_poset_of_events(intros)

(* specialised to the case of one obs - the last one in the list *)
let remove_obs p =
  let obs_id = List.length p.events in
  let events = List.filter (fun e -> not (e.event_id = obs_id)) p.events in
  let prec_1 = List.filter (fun (e1,e2) -> not (e2 = obs_id) ) p.prec_1 in
  let inhibit = List.filter (fun (e1,e2) -> not (e2 = obs_id) ) p.inhibit in
  { events; prec_1; inhibit; }
