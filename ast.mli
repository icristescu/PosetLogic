type link =
  | LNK_VALUE of int
  | FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of string (* port *)
    * string (*agent_type*)

type internal = string list

type port = {port_nme:string;
             port_int:internal;
             port_lnk:link list;}

type agent = (string * port list)

type mixture = agent list

type rule = {
  lhs: mixture ;
  bidirectional:bool ;
  rhs: mixture ;
}
type t = INIT of mixture
       | OBS of string*mixture
       | RULE of string*rule

val empty : t

val print_port : port -> unit
val print : t -> unit

val get_label : t -> String.t
val get_rule_by_label : String.t -> t list -> t

val get_lhs : t -> mixture
val get_rhs : t -> mixture
val is_init : t -> bool
val is_obs : t -> bool
