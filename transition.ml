open Lib

type t = {
    lhs: Idgraph.mixture;
    rhs: Idgraph.mixture;
  }

let empty = {lhs =Idgraph.empty;rhs =Idgraph.empty;}

let print t = Format.printf "\ntransition\n"; Idgraph.print t.lhs;
              Format.printf " => ";Idgraph.print t.rhs

let get_rhs t = t.rhs

(*
let get_int_lnk = function
  | Rule.INT s -> ([s],[])
  | Rule.LNK lnk -> ([],[lnk])

let add_ports portls pid il pname =
  let (found,new_ports) =
    List.fold_left
      (fun (ok,ports) port ->
        if (port.Idgraph.port_id = pid) then
          let (intls,lnkls) = get_int_lnk il in
          let new_port = make_port pname pid
                                   (port.Idgraph.port_int@intls)
                                   (port.Idgraph.port_lnk@lnkls) in
          (true,new_port::ports)
        else (ok,port::ports))
      (false,[]) portls in
  if found then new_ports
  else
    let (intls,lnkls) = get_int_lnk il in
    (make_port pname pid intls lnkls )::portls

let combine_ports_name quarks1 quarks2 p1 p2 =
  let quarks1'= Rule.filter_on_port p1 quarks1 in
  let quarks2'= Quark.filter_on_port p2 quarks2 in
  if ((List.length quarks1') = (List.length quarks2')) then
    List.fold_left
      (fun ok q1 ->
        match Rule.test_il q1 with
        | Some il -> (Quark.exists_test quarks2' il)&&ok
        | None -> match Rule.testmod_il q1 with
                  | Some il -> (Quark.exists_testmod quarks2' il)&&ok
                  | None -> match Rule.mod_il q1 with
                            |Some il -> (Quark.exists_mod quarks2' il)&&ok
                            |None ->
                              (raise (ExceptionDefn.Internal_Error
                                        ("combine_ports_name"))))
      true quarks1'
  else false

let combine_nodes_name node_names1 node_names2 (n1:int) (n2:int) =
  let try_find_name n node_names =
    try
      let (_,name) = (List.find (fun (n',_) -> (n'=n)) node_names) in
      Some name
    with _ -> None in
  let name1= try_find_name n1 node_names1 in
  let name2= try_find_name n2 node_names2 in
  match (name1,name2) with
  | Some name1', Some name2' -> String.equal name1' name2'
  | _, None -> true
  | None, _ -> (raise (ExceptionDefn.Internal_Error
                       ("combine_ports_name")))

let aux n1 n2 quarks1 quarks2 =
  let quarks1'= Rule.filter_on_node n1 quarks1 in
  let ports1 = Rule.get_ports quarks1 in
  let quarks2'= Quark.filter_on_node n2 quarks2 in
  let ports2 = Quark.get_ports quarks2 in
  let () = if (not((List.length quarks1') = (List.length quarks2'))) then
             (raise (ExceptionDefn.Mappings())) in
  let combine_all = mapping ports1 ports2
                            (combine_ports_name quarks1' quarks2') in
  combine_all

let combine_quarks quarks1 node_names1 quarks2 node_names2 =
  let nodes1 = Rule.get_nodes quarks1 in
  let nodes2 = Quark.get_nodes quarks2 in
  let () = if (not((List.length nodes1) = (List.length nodes2))) then
             (raise (ExceptionDefn.NotKappa_Poset
                       ("combine_quarks: quarks of event not valid"))) in
  let combine_all = mapping nodes1 nodes2
                            (combine_nodes_name node_names1 node_names2) in

  List.fold_left
    (fun acc nmap ->
      try
        let all_maps_for_nmap =
          List.map
            (fun (n1,n2) ->
              let pmaps = aux n1 n2 quarks1 quarks2 in
              (n1,n2,pmaps)) nmap in
        all_maps_for_nmap::acc
      with ExceptionDefn.Mappings() -> acc) [] combine_all

let print_all_maps maps =
  List.iter
    (fun ls1 ->
      Format.printf "\ncombine quarks resulting in map = ";
      List.iter
        (fun (n1,n2,lsls) ->
          Format.printf "  (%d,%d,[" n1 n2;
          List.iter (fun ls2 ->
                      List.iter (fun (p1,p2) ->
                                  Format.printf "(%d,%d)" p1 p2) ls2) lsls;
          Format.printf "])") ls1) maps

let decorate combinations quarks node_names node_map port_map =
  List.fold_left
    (fun acc (n1,n2,pmaps) ->
      let one_deco_ports = List.hd pmaps in
      List.fold_left
        (fun (quarks,new_nodes,new_ports) (p1,p2) ->
          let new_quarks = Rule.find_replace (n1,n2) (p1,p2) quarks in
          if (List.exists (fun (n,_) -> n=n2) node_names) then
            (new_quarks,new_nodes,new_ports)
          else
            let (nname,pname) = Maps.get_names n1 p1 node_map port_map in
            if (List.mem (n2,nname) new_nodes) then
              let ports =
                List.map
                  (fun (n,plist) ->
                    if (n=n2) then
                      (n,(p2,pname)::plist)
                    else (n,plist)) new_ports in
              (new_quarks,new_nodes,ports)
            else
              (new_quarks,(n2,nname)::new_nodes,(n2,[(p2,pname)])::new_ports))
        acc one_deco_ports)
    (quarks,[],[]) combinations



let inclusion_ports pid il plist =
  let match_ports intls lnkls = match il with
    | Rule.INT s -> List.mem s intls
    | Rule.LNK lnk -> List.mem lnk lnkls in
  List.exists
    (fun port ->
      (port.Idgraph.port_id = pid)&&
        (match_ports port.Idgraph.port_int port.Idgraph.port_lnk))
    plist

let modify_ports pid il plist =
  List.map
    (fun port ->
      if (port.Idgraph.port_id = pid) then
        match il with
        | Rule.INT s ->
           make_port port.Idgraph.port_nme pid [s] port.Idgraph.port_lnk
        | Rule.LNK lnk ->
           make_port port.Idgraph.port_nme pid port.Idgraph.port_int [lnk]
      else port) plist

let make_agent aname aid pname pid il =
  let (intls,lnkls) = get_int_lnk il in
  let port = make_port pname pid intls lnkls in
     (aname,aid,[port])

let make_new_idgraph quarks node_names port_names =
  List.fold_left
    (fun mixt q ->
      match q with
      | Rule.Modified (aid,pid,il) ->
         let (aname,pname) = Maps.get_names aid pid node_names port_names in
         let (found,new_mixt) =
           List.fold_left
             (fun (ok,mixt') ((aname',aid',plist) as agent) ->
               if (aid = aid') then
                 let () =
                   if (not(String.equal aname' aname)) then
                     (raise (ExceptionDefn.Internal_Error("make_new_id1"))) in
                 let new_ports = add_ports plist pid il pname in
                 let new_agent = (aname,aid,new_ports) in
                 (true,new_agent::mixt')
               else (ok,agent::mixt')) (false,[]) mixt in
         if found then new_mixt
         else
           let new_agent = make_agent aname aid pname pid il in
           new_agent::mixt
      | _ -> (raise (ExceptionDefn.Internal_Error("make_new_id2")))) [] quarks


let make_idgraph quarks mixture =
  List.fold_left
    (fun mixt q ->
      let aid = Rule.get_agent q in
      List.map
        (fun ((aname',aid',plist) as agent) ->
          if (aid = aid') then
            match q with
            | Rule.Tested (_,pid,il) ->
               let () =
                 if (not(inclusion_ports pid il plist)) then
                   (raise (ExceptionDefn.Internal_Error("make_idgraph1"))) in
               agent
            | Rule.Modified (_,_,_) ->
               (raise (ExceptionDefn.Internal_Error("make_idgraph2")))
            | Rule.TestedMod (_,pid,il,il') ->
               let () =
                 if (not(inclusion_ports pid il plist)) then
                   (raise (ExceptionDefn.Internal_Error("make_idgraph3"))) in
               let new_ports = modify_ports pid il' plist in
               let new_agent = (aname',aid',new_ports) in
               new_agent
          else agent) mixt) mixture quarks
 *)
let make_port pid intls lnkls =
  { Idgraph.port_id = pid;
    Idgraph.port_int = intls;
    Idgraph.port_lnk = lnkls;}

let check_tests graph tests = true

let rec replace_port pid port_lnk = function
  | p::ls -> if (p.Idgraph.port_id = pid) then
               (make_port pid p.Idgraph.port_int [port_lnk])::ls
             else p::(replace_port pid port_lnk ls)
  | [] -> []

let rec replace_free agents aid pid =
  match agents with
  | (aid',atype,plist)::ls ->
     if (aid = aid') then
       let new_ports = replace_port pid (Idgraph.FREE) plist in
       (aid,atype,new_ports)::ls
     else (aid',atype,plist)::(replace_free ls aid pid)
  | [] -> []

let replace_bind agents aid pid aid' pid' lnk_nb =
  let port_lnk = Idgraph.LNK_VALUE lnk_nb in
  let rec aux_replace_bind = function
    | (aid'',atype,plist)::ls ->
       if (aid = aid'') then
         let new_ports = replace_port pid port_lnk plist in
         (aid,atype,new_ports)::(aux_replace_bind ls)
       else (if (aid' = aid'') then
               let new_ports = replace_port pid port_lnk plist in
               (aid,atype,new_ports)::(aux_replace_bind ls)
             else (aid'',atype,plist)::(aux_replace_bind ls))
    | [] -> [] in
  aux_replace_bind agents

let get_lnknb graph = 0

let make_idgraph graph (alist,blist) node_names port_names =
  let lnknb = get_lnknb graph in
  let create_agents =
    List.fold_left
      (fun acc action ->
        match action with
          Instantiation.Create ((aid,atype),pls) ->
          let ports = List.map
                        (fun (pid,internal) ->
                          let intlist = match internal with
                            | None -> []
                            | Some intern-> [intern] in
                          make_port pid intlist []) pls in
          (aid,atype,ports)::acc
        | _ -> acc ) [] alist in
  let (agents,_) =
    List.fold_left
      (fun (acc,lnk_nb) action ->
        match action with
          Instantiation.Create _ -> (acc,lnk_nb)
        | Instantiation.Free ((aid,atype),site_name) ->
           (replace_free create_agents aid site_name,lnk_nb)
        | Instantiation.Bind_to (((aid,atype),site),((aid',_),site'))->
           (replace_bind create_agents aid site aid' site' lnk_nb,(lnk_nb+1))
        | Instantiation.Mod_internal _ | Instantiation.Remove _
          | Instantiation.Bind _ -> (acc,lnk_nb)
           (*(raise (ExceptDefn.NotKappa_Poset
                                              ("init event not valid")))*)
      )
      ([],lnknb) alist in
  create_agents@agents


let make lhs step node_names port_names =
  let tests = Trace.tests_of_step step in
  let actions = Trace.actions_of_step step in
  let rhs =
    if (check_tests lhs tests) then
      make_idgraph lhs actions node_names port_names
    else (raise (ExceptDefn.Internal_Error
                 ("make transition"))) in

  let () = if (!Param.debug_mode) then
             (Format.printf "\n\nmake transition";
              Trace.print_step Format.std_formatter step;
              Format.printf "with new nodes and ports ";
              (*Maps.printn newn; Maps.printp newp;*)
              Format.printf "\nlhs = "; Idgraph.print lhs;
              Format.printf "\nrhs = "; Idgraph.print rhs) in

  ({lhs;rhs;},node_names,port_names)
