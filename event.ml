open Yojson
open Lib

(*module IntMap = Map.Make(struct type t = int let compare = compare end)*)

type quark =
  | Tested of (int*int*int)
  | Modified of (int*int*int)
  | TestedMod of (int*int*int)

type t = {
    event_id : int; (* local id inside a story *)
    event_label : string;
    quarks : quark list;
  }

let get_id e = e.event_id
let get_label e = e.event_label
let get_quarks e = e.quarks

let test_event event_id event_label =
 { event_id; event_label; quarks = []; }

let print_event e =
  Format.printf " (%i, %s)  " (e.event_id) (e.event_label)

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

let quarks_tested qs =
  List.filter (function
                 Tested _ -> true
               | Modified _ | TestedMod _ -> false) qs

let quarks_testedMod qs =
  List.filter (function
                 TestedMod _ -> true
               | Modified _ | Tested _ -> false) qs

let quarks_modified qs =
  List.filter (function
                 Modified _ -> true
               | Tested _ | TestedMod _ -> false) qs

let get_nodes qs =
  List.fold_left
    (fun nodes -> function
      | Modified (ni,pi,il)| Tested (ni,pi,il)| TestedMod (ni,pi,il) ->
         if (List.mem ni nodes) then nodes else ni::nodes) [] qs

let get_nodes_ports qs =
  let n = List.length (get_nodes qs) in
  let myhash = Hashtbl.create n in

  let build_port_list n p qs =
    let (qs_np,rest)=
      List.partition
        (function
         | Modified (ni,pi,_)| Tested (ni,pi,_)| TestedMod (ni,pi,_) ->
            (n=ni)&&(p=pi)) qs in
    let plist =
      List.map
        (function Modified t| Tested t| TestedMod t -> Lib.thd t) qs_np in
    (plist,rest) in

  let rec build_map qs = match qs with
    | [] -> myhash
    | q::rs -> match q with
               | Modified (ni,pi,_)| Tested (ni,pi,_)| TestedMod (ni,pi,_) ->
                  let (port,rest) = build_port_list ni pi qs in
                  let () = if (pi >= 0) then Hashtbl.add myhash ni (pi,port) in
                  build_map rest in
  build_map qs
