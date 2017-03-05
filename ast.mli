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

type lnk_or_int = INT of string
                | LNK of link

type q = int*string*lnk_or_int

type quarks = Tested of q list
            | TestedMod of (q*q) list
            | Modified of q list
type rule = {
  lhs: mixture ;
  bidirectional:bool ;
  rhs: mixture ;
  prefix_map : quarks list ;
}

type t = INIT of mixture
       | OBS of string*mixture
       | RULE of string*rule

val empty : t

val print_port : port -> unit
val print : t -> unit

(* the parser returns the empty list for a free link
   we replace the empty list in link with [FREE] *)
val clean_rules : t -> t

val get_label : t -> String.t
val get_rule_by_label : String.t -> t list -> t

val get_lhs : t -> mixture
val get_rhs : t -> mixture
val is_init : t -> bool
val is_obs : t -> bool
