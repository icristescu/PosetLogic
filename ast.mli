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

(* the parser returns the empty list for a free link
   we replace the empty list in link with [FREE] *)
val clean_rule : t -> Rule.t
