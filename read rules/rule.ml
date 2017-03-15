type lnk_or_int = INT of string
                | LNK of Idgraph.link

type quark = Tested of (int*int*lnk_or_int)
            | TestedMod of (int*int*lnk_or_int*lnk_or_int)
            | Modified of (int*int*lnk_or_int)

type rule = string*(quark list)*Maps.node_map*Maps.port_map

type t = INIT of rule
       | OBS of rule
       | RULE of rule

let empty = RULE ("empty",[],[],[])

let is_init = function
  | INIT _ -> true
  | _ -> false

let get_quarks = function
  | INIT (_,qs,_,_) | OBS (_,qs,_,_) | RULE (_,qs,_,_) -> qs

let get_node_map = function
  | INIT (_,_,nm,_) | OBS (_,_,nm,_) | RULE (_,_,nm,_) -> nm

let get_port_map = function
  | INIT (_,_,_,nm) | OBS (_,_,_,nm) | RULE (_,_,_,nm) -> nm

let get_agent = function
    Tested (ag,_,_) | TestedMod (ag,_,_,_) | Modified (ag,_,_) -> ag

let get_port = function
    Tested (_,p,_) | TestedMod (_,p,_,_) | Modified (_,p,_) -> p

let get_il = function
    Tested (_,_,il) | TestedMod (_,_,il,_) | Modified (_,_,il) -> il

let print_quarks qlist nmap pmap =
  let print_triple (n,p,il) =
    (try
       let (_,agent_name) = List.find (fun (id,na) -> id=n) nmap in
       Format.printf "(%s%d," agent_name n;
       (try
          let (_,plist) = List.find (fun (id,plist) -> id=n) pmap in
          let (_,port_name) = List.find (fun (idp,na) -> idp=p) plist in
          Format.printf "%s%d," port_name p;
        with _ -> Format.printf "%d" p);
     with _ -> Format.printf "(%d,%d" n p);
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
    | TestedMod (a,b,c,d) ->
       Format.printf "\nTestedMod ";
       Format.printf"[before = ";print_triple (a,b,c);Format.printf "]  ";
       Format.printf"[after = ";print_triple (a,b,d);Format.printf "]  "
  in
  List.iter (fun q -> print_q q) qlist

let print t =
  let print_rule (label,qs,nmap,pmap) =
    Format.printf "rule %s = " label; print_quarks qs nmap pmap in
  match t with
  | INIT r ->  Format.printf "\ninit ";print_rule r
  | OBS r ->  Format.printf "\nobs ";print_rule r
  | RULE r ->  Format.printf "\nrule ";print_rule r


let get_label = function
  | INIT (name,_,_,_) | OBS (name,_,_,_) | RULE (name,_,_,_) ->  name

let get_rule_by_label nme rules =
  List.find (fun r -> String.equal (get_label r) nme) rules

let filter_on_port p quarks =
  List.filter (fun q -> (get_port q)=p) quarks

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
let test_il = function Tested (_,_,il) -> (match il with
                                           | INT _ -> Some [0]
                                           | LNK _ -> Some [1])
                     | _ -> None
let testmod_il = function TestedMod (_,_,il,_) -> (match il with
                                                     | INT _ -> Some [0]
                                                     | LNK _ -> Some [1])
                        | _ -> None
let mod_il = function Modified (_,_,il) -> (match il with
                                            | INT _ -> Some [0]
                                            | LNK _ -> Some [1])
                    | _ -> None

let find_replace (n1,n2) (p1,p2) quarks =
  List.map (function
            |Tested (n,p,il) as q -> if ((n=n1)&&(p1=p)) then
                                       Tested (n2,p2,il) else q
            |TestedMod (n,p,il,il') as q ->
                        if ((n=n1)&&(p1=p)) then
                          TestedMod (n2,p2,il,il') else q
            |Modified (n,p,il) as q -> if ((n=n1)&&(p1=p)) then
                                         Modified (n2,p2,il) else q) quarks
