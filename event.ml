open Yojson
open Lib

type t = {
    event_id : int; (* local id inside a story *)
    event_label : string;
    quarks : Quark.t list;
  }

let get_id e = e.event_id
let get_label e = e.event_label
let get_quarks e = e.quarks

let test_event event_id event_label =
 { event_id; event_label; quarks = []; }

let print_event e =
  Format.printf " (%i, %s)  " (e.event_id) (e.event_label)

let nodes_of_json (node:Yojson.Basic.json) =
  let open Yojson.Basic.Util in
  match node with
  | `List [`Int id; `String "RULE"; `String label;
           (`Assoc ["quarks", `List l]) ]
    | `List [`Int id; `String "PERT"; `String label;
             (`Assoc ["quarks", `List l])]->
     let quarks_ls =
       List.map (fun q -> Quark.quarks_of_json q) l in
     let clean_quarks =
       List.filter (fun q -> Quark.positive_site q) quarks_ls in
     { event_id = id; event_label = label; quarks = clean_quarks; }
  | `List [`Int id; `String "OBS"; `String label;
           (`Assoc ["quarks", `List l])] ->
     let quarks_ls =
       List.map (fun q -> Quark.quarks_of_json q) l in
     let clean_quarks =
       List.filter (fun q -> Quark.positive_site q) quarks_ls in
     let () = if ((Quark.exists_mod clean_quarks [0;1])
                  &&(Quark.exists_testmod clean_quarks [0;1])) then
                (raise (ExceptionDefn.NotKappa_Poset
                          ("quarks of init event not valid"))) in
     { event_id = id; event_label = label; quarks = clean_quarks; }
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
       List.map (fun q -> Quark.quarks_of_json q) ql in
     let clean_quarks =
       List.filter (fun q -> Quark.positive_site q) quarks_ls in
     let () = if ((Quark.exists_test clean_quarks [0;1])
                  &&(Quark.exists_testmod clean_quarks [0;1])) then
                (raise (ExceptionDefn.NotKappa_Poset
                          ("quarks of init event not valid"))) in
     { event_id = id; event_label = init_label; quarks = clean_quarks; }
  | _ -> raise (Yojson.Basic.Util.Type_error ("Not in the cflow format",`Null))
