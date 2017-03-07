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
  let exists_test i =
    List.exists (function Quark.Tested (_,_,il) -> il = i
                        | _ -> false) quarks2' in
  let exists_testmod i =
    List.exists (function Quark.TestedMod (_,_,il) -> il = i
                        | _ -> false) quarks2' in
  let exists_mod i =
    List.exists (function Quark.Modified (_,_,il) -> il = i
                        | _ -> false) quarks2' in
  if ((List.length quarks1') = (List.length quarks2')) then
    List.fold_left
      (fun ok q1 ->
        match q1 with
          Rule.Tested (_,_,il) ->
          (match il with
          | Rule.INT _ -> (exists_test 0)&&ok
          | Rule.LNK _ -> (exists_test 1)&&ok)
         |Rule.TestedMod ((_,_,il),_) ->
           (match il with
            | Rule.INT _ -> (exists_testmod 0)&&ok
            | Rule.LNK _ -> (exists_testmod 1)&&ok)
         |Rule.Modified (_,_,il) ->
           (match il with
            | Rule.INT _ -> (exists_mod 0)&&ok
            | Rule.LNK _ -> (exists_mod 1)&&ok))
      true quarks1'
  else false

let combine_nodes_name node_names1 node_names2 (n1:int) (n2:int) =
  let (_,name1) = List.find (fun (n',_) -> (n'=n1)) node_names1 in
  let (_,name2) = List.find (fun (n',_) -> (n'=n2)) node_names2 in
  String.equal name1 name2

let aux n1 n2 quarks1 quarks2 =
  let quarks1'= Rule.filter_on_node n1 quarks1 in
  let ports1 = Rule.get_ports quarks1 in
  let quarks2'= Quark.filter_on_node n2 quarks2 in
  let ports2 = Quark.get_ports quarks2 in
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
                       ("combine: quarks of event not valid"))) in
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

(*let decorate1 (quarksA:Ast.quarks) (quarksQ:Quark.t) = []*)

let make lhs rule quarks node_names port_names =
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
