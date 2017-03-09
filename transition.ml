open Lib

type t = {
    lhs: Idgraph.mixture;
    rhs: Idgraph.mixture;
    rule : Rule.t;
  }

let empty = {lhs =Idgraph.empty;rhs =Idgraph.empty;rule=Rule.empty}

let print t = Format.printf "\n transition"; Idgraph.print t.lhs;
              Format.printf " => ";Idgraph.print t.rhs; Rule.print t.rule

let get_rhs t = t.rhs

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
  |Some name1', Some name2' -> String.equal name1' name2'
  |_,None -> true
  |None,_ -> (raise (ExceptionDefn.Internal_Error
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

let add_ports portls pid il pname =
  let (found,new_ports) =
    List.fold_left
      (fun (ok,ports) port ->
        if (port.Idgraph.port_id = pid) then (true,port::ports)
        else (ok,port::ports)) (false,[]) portls in
  if found then new_ports
  else (Rule.make_port pid il pname)::portls

let new_idgraph quarks node_names port_names =
  List.fold_left
    (fun mixt q ->
      let aid = Rule.get_agent q in
      let pid = Rule.get_port q in
      let il = Rule.get_il q in
      let (aname,pname) = Maps.get_names aid pid node_names port_names in
      let (found,new_mixt) =
        List.fold_left
          (fun (ok,mixt') ((agentn,agenti,plist) as agent) ->
            if (aid = agenti) then
              let () = if (not(String.equal agentn aname)) then
                         (raise (ExceptionDefn.Internal_Error("make_agent"))) in
              let new_ports = add_ports plist pid il pname in
              let new_agent = (agentn,agenti,new_ports) in
              (true,new_agent::mixt')
            else (ok,agent::mixt')) (false,[]) mixt in
      if found then new_mixt
      else
        let new_agent = Rule.make_agent aname pname q in
        new_agent::mixt) [] quarks
(*
let make_idgraph quarks node_names port_names mixture =
  List.fold_left
    (fun mixt q ->
    ) [] quarks
 *)


let make lhs rule quarks node_names port_names =
  let () = Format.printf "\nmake transition";
           Rule.print rule;
           List.iter (fun q-> Quark.print q) quarks in
  let node_map = Rule.get_node_map rule in
  let port_map = Rule.get_port_map rule in
  let rule_quarks = Rule.get_quarks rule in
  let all_maps = combine_quarks rule_quarks node_map quarks node_names in
  let () = print_all_maps all_maps in
  let one_deco_agents = List.hd all_maps in
  let (new_quarks,newn,newp) = decorate one_deco_agents rule_quarks
                            node_names node_map port_map in
  let () = Format.printf "\nnew quarks\n";
           Rule.print_quarks new_quarks node_map port_map;
           Format.printf "with new nodes and ports ";
           Maps.printn newn; Maps.printp newp in
  let nodes = newn@node_names in
  let ports = newp@port_names in
  let new_nodes =
    List.filter (fun q ->
                  let id = Rule.get_agent q in
                  (List.exists (fun (nid,_) -> nid = id) newn)) new_quarks in
  let rhs = new_idgraph new_nodes newn newp in
  let () = Format.printf "rhs = "; Idgraph.print rhs in
  ({lhs;rhs=lhs;rule},nodes,ports)
