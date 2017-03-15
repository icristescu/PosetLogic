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
  bidirectional: bool ;
  rhs: mixture ;
}

type t = INIT of mixture
       | OBS of string*mixture
       | RULE of string*rule

let print_link = function
  | LNK_VALUE i -> Format.printf "!%d" i
  | FREE -> Format.printf "free"
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
  | RULE (name,r) -> Format.printf "\n rule '%s' " name; print_rule r

let empty_rule = {lhs =[];rhs=[];bidirectional=false;}
let empty = RULE ("empty",empty_rule)


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

let split_port_quarks id_node id_port port =
  let quark_int = match port.port_int with
    | [] -> []
    | s::_ -> [(id_node,id_port,(Rule.INT s))] in
  match port.port_lnk with
  | [] -> []
  | l::_ -> match l with
            | LNK_VALUE i ->
               let rule_lnk = Rule.LNK (Idgraph.LNK_VALUE i) in
               (id_node,id_port,rule_lnk)::quark_int
            | FREE ->
               let rule_lnk = Rule.LNK (Idgraph.FREE) in
               (id_node,id_port,rule_lnk)::quark_int
            | LNK_ANY -> quark_int
            | LNK_SOME | LNK_TYPE _ ->
               (raise (ExceptionDefn.Not_Supported
                         ("rules with side effects not supported")))

let split_ports_in_quarks count plist =
  let (_,qs) =
    List.fold_left (fun (i,acc) port ->
                     (i+1,(split_port_quarks count i port)@acc))
                   (0,[]) plist in
  qs

let create_quarks mixture count =
  List.fold_left
    (fun (qs,agent_names,port_names,count) ((name:string),plist) ->
      let qs' = split_ports_in_quarks count plist in
      let ports = List.mapi (fun i p -> (i,p.port_nme)) plist in
      (qs'@qs,(count,name)::agent_names,(count,ports)::port_names,count+1))
    ([],[],[],count) mixture

let create_rhs_quarks agent_names port_names mixture =
  let rec aux qs mixt count ag_nm po_nm =
    match mixt with
    | (name,plist)::mixt' ->
       if (((List.length ag_nm) >0)&&(List.mem (count,name) ag_nm)) then
         let qs' = split_ports_in_quarks count plist in
         aux (qs'@qs) mixt' (count+1) ag_nm po_nm
       else
         let (qs',ag_nm',po_nm',_) = create_quarks mixt (List.length ag_nm) in
         (qs'@qs, ag_nm'@ag_nm, po_nm')
    | [] -> (qs, ag_nm, po_nm) in
  aux [] mixture 0 agent_names port_names

let match_il il il' = match (il,il') with
  | (Rule.INT _,Rule.INT _) | (Rule.LNK _,Rule.LNK _) -> true
  | _ -> false

let remove_quark (n,p,il) ls =
  List.filter
    (fun (n',p',il') ->
      (not((n=n')&&(p=p')&&(match_il il il')))) ls

let partition_quarks lhs_quarks rhs_quarks =
  let find (n,p,il) rhs =
    List.find (fun (n',p',il') ->
                (n=n')&&(p=p')&&(match_il il il')) rhs in
  let (qlist,rhs)=
    List.fold_left
      (fun (qlist',rhs') (n,p,il) ->
        if (List.mem (n,p,il) rhs') then
          let new_quark = Rule.Tested (n,p,il) in
          (new_quark::qlist',(remove_quark (n,p,il) rhs'))
        else
          let (_,_,il') =
            (try (find (n,p,il) rhs')
             with _ ->
               (raise
                  (ExceptionDefn.Syntax_Error
                     ("agent does not have the same ports in lhs and rhs1"))))in
          let new_quark = Rule.TestedMod (n,p,il,il') in
          (new_quark::qlist',(remove_quark (n,p,il) rhs')))
      ([],rhs_quarks) lhs_quarks in
  let modified =
    List.map
      (fun ((ag,_,_) as q) ->
        let () =
          List.iter (fun q -> if (ag=(Rule.get_agent q)) then
                                (raise
                                   (ExceptionDefn.Syntax_Error
                                      ("agent does not have the same ports
                                        in lhs and rhs2")))) qlist in
               Rule.Modified q) rhs in
  (modified@qlist)

let create_prefix_map lhs rhs =
  let (lhs_quarks,lhs_an,lhs_pn,_) = create_quarks lhs 0 in
  let (rhs_quarks,rhs_an,rhs_pn) = create_rhs_quarks lhs_an lhs_pn rhs in
  let () = if (!Parameter.debug_mode) then
             (Format.printf "rhs_agent = ";
              Maps.printn rhs_an; Maps.printp rhs_pn) in
  (partition_quarks lhs_quarks rhs_quarks,rhs_an,rhs_pn)

let clean_rule = function
  | INIT mix ->
     let (quarks,agent_names,port_names) =
       create_prefix_map [] (add_free_to_mix mix) in
     let label = List.fold_left (fun acc (nme,_) -> acc^nme) "" mix in
     Rule.INIT (label,quarks,agent_names,port_names)
  | OBS (name,mix) ->
     let mix' = add_free_to_mix mix in
     let (quarks,agent_names,port_names) = create_prefix_map mix' mix'  in
     Rule.OBS (name,quarks,agent_names,port_names)
  | RULE (name,r) ->
     let () = if (r.bidirectional) then
                (raise (ExceptionDefn.Not_Supported
                          ("bidirectional rules not supported"))) in
     let lhs = add_free_to_mix r.lhs in
     let rhs = add_free_to_mix r.rhs in
     let (quarks,agent_names,port_names) = create_prefix_map lhs rhs in
     Rule.RULE (name,quarks,agent_names,port_names)
