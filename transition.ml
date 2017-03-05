open Lib

type t = {
    lhs: Idgraph.mixture;
    rhs: Idgraph.mixture;
    rule : Ast.t;
  }

let empty = {lhs =Idgraph.empty;rhs =Idgraph.empty;rule=Ast.empty}

let print t = Format.printf "\n transition"; Idgraph.print t.lhs;
              Format.printf " => ";Idgraph.print t.rhs; Ast.print t.rule

let get_rhs t = t.rhs

let combine_pname_pid (p1:Ast.port) ((pid, il): int * int list) =
  if (((List.length (p1.Ast.port_int) = 1)&&(List.mem 0 il))||
        ((List.length (p1.Ast.port_int) = 0)&&(not(List.mem 0 il)))) then
    let port_lnk =
      match p1.Ast.port_lnk with
      | [] -> []
      | l::rs -> if (not(rs=[])) then
                   (raise (ExceptionDefn.Internal_Error
                             ("port_link has at most one element")))
                 else (match l with
                         Ast.LNK_VALUE i -> [Idgraph.LNK_VALUE i]
                       | Ast.FREE -> [Idgraph.FREE]
                       | Ast.LNK_ANY | Ast.LNK_SOME | Ast.LNK_TYPE _ ->
                          (raise
                             (ExceptionDefn.Not_Supported
                                ("rules with side effects not supported")))) in
    {Idgraph.port_nme=p1.Ast.port_nme;port_id = pid;
     port_int=p1.Ast.port_int;port_lnk}
  else (raise (ExceptionDefn.Mappings()))

let mapping list1 list_of_list combine =
  List.fold_left
    (fun acc list2 ->
      try
        let working_map = List.map2 (fun p1 p2 -> combine p1 p2) list1 list2 in
        working_map::acc
      with ExceptionDefn.Mappings() -> acc) [] list_of_list

let combine_ag_nd node_hash (name,aplist) node =
  let plist = Hashtbl.find_all node_hash node in
  let () = if (!Parameter.debug_mode) then
             (Format.printf "combine name %s node %d ports in rule " name node;
              List.iter (fun p -> Ast.print_port p) aplist;
              Format.printf "\n ports in quark";
              List.iter
                (fun (p,il) -> Format.printf " p = %d" p;
                               List.iter (fun i -> Format.printf "~%d" i) il)
                plist) in
  let _ =
    if (not((List.length aplist) = (List.length plist)))
    then (raise (ExceptionDefn.Mappings())) in
  let combine_ports = mapping aplist (permutations plist)
                              (combine_pname_pid) in
  let m = List.hd combine_ports in
  (name,node,m)

let decorate agents quarks rname =
  let nodes = Quark.get_nodes quarks in
  let () = if (!Parameter.debug_mode) then
             (Format.printf "\n decorate %s nodes = " rname;
              List.iter (fun i ->Format.printf "%d " i) nodes) in
  let _ =
    if (not((List.length nodes) = (List.length agents))) then
      (raise (ExceptionDefn.NotKappa_Poset
                ("decorate: quarks of event "
                 ^rname^" not valid"))) in
  let node_hash = Quark.get_nodes_ports quarks in
  let combine_agents = mapping agents (permutations nodes)
                               (combine_ag_nd node_hash) in
  let m = List.hd combine_agents in
  m

let make lhs rule quarks =
  let quarks_tested = Quark.quarks_tested quarks in
  let quarks_testedMod = Quark.quarks_testedMod quarks in
  let quarks_modified = Quark.quarks_modified quarks in
  if (Ast.is_init rule) then
    let _ =
      if (not((List.length quarks_tested)=0))||
           (not((List.length quarks_testedMod)=0)) then
        (raise (ExceptionDefn.NotKappa_Poset
                  ("make transition: quarks of event "
                   ^(Ast.get_label rule)^" not valid"))) in
    let rhs_decor =
      decorate (Ast.get_rhs rule) quarks_modified (Ast.get_label rule) in
    {lhs;rhs=rhs_decor@lhs;rule}
  else
    if (Ast.is_obs rule) then
      let _ =
        if (not((List.length quarks_modified)=0))||
             (not((List.length quarks_testedMod)=0)) then
          (raise (ExceptionDefn.NotKappa_Poset
                    ("make transition: quarks of event "
                     ^(Ast.get_label rule)^" not valid"))) in
      {lhs;rhs=lhs;rule}
    else {lhs;rhs=lhs;rule}
