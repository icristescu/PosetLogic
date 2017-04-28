open Yojson
open Lib

type t = {
    event_id : int; (* local id inside a story *)
    rule_id : int; (* id of primitive rule *)
    event_label : string; (*label of corresponding ast rule *)
    step : Trace.step ;
  }

let id e = e.event_id
let rule_id e = e.rule_id
let label e = e.event_label
let step e = e.step

let test_event event_id event_label =
 { event_id; rule_id = -1; event_label; step = Trace.Dummy "none"; }

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
     { event_id = id; rule_id; event_label; step;}
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect node of json",x))
