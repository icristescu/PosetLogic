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

let mapping list1 list_of_list combine =
  List.fold_left
    (fun acc list2 ->
      try
        let working_map =
          List.map2 (fun p1 p2 ->
                      if (combine p1 p2) then (p1,p2)
                      else (raise (ExceptionDefn.Mappings()))) list1 list2 in
        working_map::acc
      with ExceptionDefn.Mappings() -> acc) [] list_of_list

let combine_ports_name quarks1 quarks2 (p1:string) (p2:int) =
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
  let combine_all = mapping ports1 (permutations ports2)
                            (combine_ports_name quarks1' quarks2') in
  combine_all

let combine_quarks quarks1 node_names1 quarks2 node_names2 =
  let nodes1 = Rule.get_nodes quarks1 in
  let nodes2 = Quark.get_nodes quarks2 in
  let () = if (!Parameter.debug_mode) then
             (Format.printf "\n combine_quarks ") in
  let () = if (not((List.length nodes1) = (List.length nodes2))) then
             (raise (ExceptionDefn.NotKappa_Poset
                       ("combine_quarks: quarks of event not valid"))) in
  let combine_all = mapping nodes1 (permutations nodes2)
                            (combine_nodes_name node_names1 node_names2) in
  List.map
    (fun nmap ->
      let all_maps_for_nmap =
        List.map
          (fun (n1,n2) ->
            let pmaps = aux n1 n2 quarks1 quarks2 in
            if ((List.length pmaps) = 0) then (raise (ExceptionDefn.Mappings()))
            else (n1,n2,pmaps)) nmap in
      all_maps_for_nmap) combine_all

let print_all_maps maps =
  List.iter
    (fun ls1 ->
      List.iter
        (fun (n1,n2,lsls) ->
          Format.printf "  (%d,%d,[" n1 n2;
          List.iter (fun ls2 ->
                      List.iter (fun (p1,p2) ->
                                  Format.printf "(%s,%d)" p1 p2) ls2) lsls;
          Format.printf "])") ls1) maps

let decorate quarks1 node_names1 quarks2 node_names2=
  let all_maps = combine_quarks quarks1 node_names1 quarks2 node_names2 in
  let () = print_all_maps all_maps in
  let one_deco_agents = List.hd all_maps in
  List.fold_left
    (fun acc (n1,n2,pmaps) ->
      let one_deco_ports = List.hd pmaps in
      List.fold_left
        (fun accp (p1,p2) -> Rule.find_replace (n1,n2) (p1,p2) accp)
        acc one_deco_ports) quarks1 one_deco_agents

let make lhs rule quarks node_names port_names =
  let () = Format.printf "make transition\n";
           Rule.print rule;
           List.iter (fun q-> Quark.print q) quarks in
  let node_map = Rule.get_node_map rule in
  let new_quarks = decorate (Rule.get_quarks rule) node_map
                            quarks node_names in
  let () = Format.printf "new_quarks\n";
           Rule.print_quarks new_quarks node_map in
  (*let quarks_tested = Quark.quarks_tested quarks in
  let quarks_testedMod = Quark.quarks_testedMod quarks in
  let quarks_modified = Quark.quarks_modified quarks in
  if (Rule.is_init rule) then
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
    else*)
      ({lhs;rhs=lhs;rule},[],[])
