type ('a) link =
  | LNK_VALUE of int
  | FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of 'a (* port *)
    * 'a (*agent_type*)

type internal = string list

type port = {port_nme:string;
             port_int:internal;
             port_lnk:string link list;}

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
val print : t -> unit
