type lnk_or_int = INT of string
                | LNK of Idgraph.link

type quark = Tested of (int*string*lnk_or_int)
            | TestedMod of ((int*string*lnk_or_int)*(int*string*lnk_or_int))
            | Modified of (int*string*lnk_or_int)

type rule = string*(quark list)*((int*string) list)

type t = INIT of rule
       | OBS of rule
       | RULE of rule

let empty = RULE ("empty",[],[])

let get_agent = function
    Tested (ag,_,_) | TestedMod ((ag,_,_),_) | Modified (ag,_,_) -> ag

let get_port = function
    Tested (_,p,_) | TestedMod ((_,p,_),_) | Modified (_,p,_) -> p

let print_quarks qlist nmap =
  let print_triple (n,p,il) =
    let (_,agent_name) = List.find (fun (id,na) -> id=n) nmap in
    Format.printf "(%s%d,%s," agent_name n p;
    (match il with
       INT i -> Format.printf "int=%s " i
     | LNK lnk ->
        Format.printf "lnk= "; Idgraph.print_link lnk);
    Format.printf ")" in
  let print_q = function
    | Tested triple ->
       Format.printf "\nTested: "; print_triple triple
    | Modified triple ->
       Format.printf "\nModified: "; print_triple triple
    |TestedMod (q1,q2) ->
      Format.printf "\nTestedMod ";
      Format.printf"[before = ";print_triple q1;Format.printf "]  ";
      Format.printf"[after = ";print_triple q2;Format.printf "]  "
  in
  List.iter (fun q -> print_q q) qlist

let print t =
  let print_rule (label,qs,nmap) =
    Format.printf "rule %s = " label; print_quarks qs nmap in
  match t with
  | INIT r ->  Format.printf "\ninit ";print_rule r
  | OBS r ->  Format.printf "\nobs ";print_rule r
  | RULE r ->  Format.printf "\nrule ";print_rule r


let get_label = function
  | INIT (name,_,_) | OBS (name,_,_) | RULE (name,_,_) ->  name

let get_rule_by_label nme rules =
  List.find (fun r -> String.equal (get_label r) nme) rules

(*
let get_lhs = function
  | INIT _ -> []
  | OBS (name,mix) -> mix
  | RULE (name,r) -> r.lhs

let get_rhs = function
  | INIT mix -> mix
  | OBS (name,mix) -> mix
  | RULE (name,r) -> r.rhs

let is_init = function
  | INIT _ -> true
  | OBS _ | RULE _ -> false

let is_obs = function
  | OBS _ -> true
  | INIT _ | RULE _  -> false
 *)

let filter_on_port p quarks =
  List.filter (fun q -> (String.equal (get_port q) p)) quarks

let filter_on_node n quarks =
  List.filter (fun q -> ((get_agent q)=n)) quarks

let get_nodes quarks =
  List.fold_left
    (fun nodes q ->
      let n = get_agent q in
      if (List.mem n nodes) then nodes else n::nodes) [] quarks

let get_ports quarks =
  List.fold_left
    (fun ports q ->
      let p = get_port q in
      if (List.mem p ports) then ports else p::ports) [] quarks
