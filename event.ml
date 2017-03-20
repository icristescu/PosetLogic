open Yojson
open Lib

type t = {
    event_id : int; (* local id inside a story *)
    event_label : string;
    step : Trace.step ;
  }

let get_id e = e.event_id
let get_label e = e.event_label
let get_step e = e.step

let test_event event_id event_label =
 { event_id; event_label; step = Trace.Dummy "none"; }

let print_event e =
  Format.printf "\n(%i, %s) " (e.event_id) (e.event_label)

let print_complete_event e =
  print_event e;Trace.print_step (Format.std_formatter) e.step;Format.printf"\n"

let nodes_of_json env = function
  | `List [`Int id; step_json] ->
     let step = Trace.step_of_yojson step_json in
     let env = Some env in
     let () = Trace.print_label_of_step ?env (Format.str_formatter) step in
     let label = Format.flush_str_formatter () in
     { event_id = id; event_label = label; step;}
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect node of json",x))
