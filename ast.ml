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

let print_link = function
  | LNK_VALUE i -> Format.printf "!%d" i
  | FREE -> ()
  | LNK_ANY -> Format.printf "!_"
  | LNK_SOME -> Format.printf "!_"
  | LNK_TYPE (i,a) -> Format.printf "!%s.%s" i a

let print_port p =
  Format.printf "%s~[" p.port_nme;
  List.iter (fun intern -> Format.printf "%s " intern;) p.port_int;
  Format.printf "]";
  List.iter (fun p -> print_link p) p.port_lnk

let print_agent (name,plist) =
  Format.printf "%s(" name;
  List.iter (fun p -> print_port p) plist;
  Format.printf ") "

let print_rule r =
  List.iter (fun a -> print_agent a) r.lhs;
  if (r.bidirectional) then Format.printf " <-> "
  else Format.printf " -> ";
  List.iter (fun a -> print_agent a) r.rhs

let print = function
  | INIT mix -> Format.printf "\n init "; List.iter (fun a -> print_agent a) mix
  | OBS (name,mix) -> Format.printf "\n obs '%s' " name;
                      List.iter (fun a -> print_agent a) mix
  | RULE (name,r) -> Format.printf "\n rule '%s' " name; print_rule r;
