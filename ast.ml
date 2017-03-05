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
  bidirectional: bool ;
  rhs: mixture ;
  prefix_map : quarks list;
}
type t = INIT of mixture
       | OBS of string*mixture
       | RULE of string*rule


let add_free_to_port_lnk plinks = match plinks with
  | [] -> [FREE]
  | ls -> ls

let add_free_to_mix mix =
  List.map
    (fun (n,pl) ->
      let pl' =
        List.map
          (fun p ->
            let p_lnks= add_free_to_port_lnk p.port_lnk in
            {port_nme=p.port_nme;port_int=p.port_int; port_lnk=p_lnks}) pl in
      (n,pl')) mix

let split_ports_quarks count port =
  let quark_int = match port.port_int with
    | [] -> []
    | s::_ -> [(count,port.port_nme,(INT s))] in
  match port.port_lnk with
  | [] -> []
  | l::_ -> (count,port.port_nme,(LNK l))::quark_int

let create_quarks mixture count =
  List.fold_left
    (fun (qs,agent_names,count) (name,plist) ->
      let qs' =
        List.fold_left
          (fun acc port -> (split_ports_quarks count port)@acc) [] plist in
      (qs'@qs,(name,count)::agent_names,count+1))
    ([],[],count) mixture

let create_rhs_quarks agent_names mixture =
  let rec aux qs mixt count ag_nm =
    match mixt with
    | (name,plist)::mixt' ->
       if (List.mem (name,count) ag_nm) then
         let qs' =
           List.fold_left
             (fun acc port -> (split_ports_quarks count port)@acc) [] plist in
         aux (qs'@qs) mixt' (count+1) ag_nm
       else
         let (qs',ag_nm',_) = create_quarks mixt' (List.length ag_nm) in
         (qs'@qs, ag_nm'@ag_nm)
    | [] -> (qs, []) in
  aux [] mixture 0 agent_names

let match_il il il' = match (il,il') with
  | (INT _,INT _) | (LNK _,LNK _) -> true
  | _ -> false

let remove_quark (n,p,il) ls =
  List.filter
    (fun (n',p',il') ->
      (not((n=n')&&(String.equal p p')&&(match_il il il')))) ls

let partition_quarks lhs_quarks rhs_quarks =
  let find (n,p,il) rhs =
    List.find (fun (n',p',il') ->
                (n=n')&&(String.equal p p')&&(match_il il il')) rhs in
  let (tested,tested_mod,rhs)=
    List.fold_left
      (fun (tested,tested_mod,rhs) (n,p,il) ->
        if (List.mem (n,p,il) rhs_quarks) then
          (((n,p,il)::tested),tested_mod,(remove_quark (n,p,il) rhs))
        else
          let (n',p',il') =
            (try (find (n,p,il) rhs)
             with _ ->
               (raise
                  (ExceptionDefn.Syntax_Error
                     ("agent does not have the same ports in lhs and rhs")))) in
          (tested,((n,p,il),(n',p',il'))::tested_mod,
           (remove_quark (n,p,il) rhs)))
      ([],[],rhs_quarks) lhs_quarks in
  let () =
    List.iter (fun (ag,_,_) ->
                List.iter
                  (fun (ag',_,_) ->
                    if (ag=ag') then
                      (raise
                         (ExceptionDefn.Syntax_Error
                            ("agent does not have the same ports
                              in lhs and rhs")))) rhs)
              tested in
  (tested,tested_mod,rhs)

let create_prefix_map lhs rhs =
  let (lhs_quarks,lhs_agent_nm,_) = create_quarks lhs 0 in
  let (rhs_quarks,rhs_agent_nm) = create_rhs_quarks lhs_agent_nm rhs in
  let (t_quarks,tm_quarks,m_quarks) =
    partition_quarks lhs_quarks rhs_quarks in
  [Tested t_quarks; TestedMod tm_quarks; Modified m_quarks]

let clean_rules = function
  | INIT mix -> INIT (add_free_to_mix mix)
  | OBS (name,mix) -> OBS (name,add_free_to_mix mix)
  | RULE (name,r) ->
     let () = if (r.bidirectional) then
                (raise (ExceptionDefn.Not_Supported
                          ("bidirectional rules not supported"))) in
     let lhs = add_free_to_mix r.lhs in
     let rhs = add_free_to_mix r.rhs in
     let r' = {lhs; bidirectional=r.bidirectional;rhs;
               prefix_map = (create_prefix_map lhs rhs)} in
     RULE (name,r')

let print_link = function
  | LNK_VALUE i -> Format.printf "!%d" i
  | FREE -> Format.printf "free"
  | LNK_ANY -> Format.printf "!_"
  | LNK_SOME -> Format.printf "!_"
  | LNK_TYPE (i,a) -> Format.printf "!%s.%s" i a

let print_quarks qs =
  let print_q (n,p,il) =
    Format.printf "(%d,%s," n p;
    (match il with
      INT i -> Format.printf "int=%s " i
    | LNK lnk ->
       Format.printf "lnk= "; print_link lnk);
    Format.printf ")" in
  let print_qlist qlist =
    List.iter (fun q -> print_q q) qlist in
  match qs with
  | Tested qlist ->
     Format.printf "\nTested: "; print_qlist qlist
  | Modified qlist ->
     Format.printf "\nModified: "; print_qlist qlist
  |TestedMod qqlist ->
    Format.printf "\nTestedMod ";
    List.iter (fun (q1,q2) ->
                Format.printf"[before = ";print_q q1;Format.printf "]  ";
                Format.printf"[after = ";print_q q2;Format.printf "]  ") qqlist

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
  List.iter (fun a -> print_agent a) r.rhs;
  List.iter (fun quarks -> print_quarks quarks) r.prefix_map

let print = function
  | INIT mix -> Format.printf "\n init "; List.iter (fun a -> print_agent a) mix
  | OBS (name,mix) -> Format.printf "\n obs '%s' " name;
                      List.iter (fun a -> print_agent a) mix
  | RULE (name,r) -> Format.printf "\n rule '%s' " name; print_rule r

let empty_rule = {lhs =[];rhs=[];bidirectional=false;prefix_map=[]}

let empty = RULE ("empty",empty_rule)

let get_label = function
  | INIT mix -> List.fold_left (fun acc (nme,_) -> acc^nme) "" mix
  | OBS (name,_) -> name
  | RULE (name,_) ->  name

let get_rule_by_label nme rules =
  List.find
    (fun r -> String.equal (get_label r) nme) rules

let get_lhs = function
  | INIT mix -> []
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
