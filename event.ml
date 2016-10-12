open Yojson

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
