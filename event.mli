open Yojson

type c =
  (((int*int) list * Pattern.cc * Pattern.id) list * (* the context *)
     ((int*int) list * Pattern.cc * Pattern.id) list * (* the lhs of the rule *)
     (int * int) list * (* the morphism abstract_event -> concrete_event *)
       (*((int * int) list* Pattern.id) list * *)
         (* the morphisms ccs_rule -> abstract_event *)
         (Instantiation.concrete Instantiation.action list))
         (*the actions - used to detect negative influence btw rules*)
type t = {
    event_id : int;
    rule_id : int;
    event_label : string;
    step : Trace.step ;
    mutable concrete : c option
  }

val nodes_of_json : Model.t option -> Yojson.Basic.json -> t

val id : t -> int
val label : t -> string
val step : t -> Trace.step
val rule_id : t -> int
val concrete : t -> c option

val print_event : t -> unit

val set_concrete :
  Model.t -> Pattern.Env.t -> Signature.s -> t ->
  ((int*int) list * Pattern.cc * Pattern.id) list -> c
