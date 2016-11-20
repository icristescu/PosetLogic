type node = {
    agent : int;
    agent_type : int;
    site : int;
  }

type edge =
  | Bound of node * node
  | Free of node

type internal_state = {
    agent_site : node;
    property : int;
  }

type site_graph = {
    nodes : node list;
    edges : edge list;
    property_set : internal_state list;
  }

let test_graph1 =
  let node0 = {
      agent = 0;
      agent_type =0;
      site = 0; } in
  let node1 = {
      agent = 0;
      agent_type =0;
      site = 1; } in
  let node2 = {
      agent = 1;
      agent_type = 0;
      site = 1; } in
  let edge12 = Bound (node1, node2) in
  let edge0 = Free node0 in
  { nodes = [node0; node1; node2]; edges = [edge12; edge0]; property_set = [] }

let test_graph2 =
  let node0 = {
      agent = 1;
      agent_type =1;
      site = 0; } in
  let node1 = {
      agent = 0;
      agent_type =0;
      site = 1; } in
  let node2 = {
      agent = 1;
      agent_type = 1;
      site = 1; } in
  let edge12 = Bound (node1, node2) in
  let edge0 = Free node0 in
  let property1 = { agent_site = node0; property = 1} in
  { nodes = [node0; node1; node2]; edges = [edge12; edge0];
    property_set = [property1] }

let print_graph graph =
  let print_node n =
    Format.printf
      "(agent_type,site) = (%d,%d)"
      n.agent_type n.site in
  let print_edge e = match e with
    | Bound (n1,n2) ->
       Format.printf "["; print_node n1; print_node n2; Format.printf "]\n"
    | Free n1 ->
       Format.printf "["; print_node n1; Format.printf "free]\n" in
  let print_internal_state i =
    Format.printf "agent_site = "; print_node i.agent_site;
    Format.printf "property = %d " i.property in
  Format.printf "nodes: ";
  List.iter (fun n -> print_node n) graph.nodes;
  Format.printf "\n edges: ";
  List.iter (fun e -> print_edge e) graph.edges;
  Format.printf "\n property set: ";
  List.iter (fun i -> print_internal_state i) graph.property_set;
  Format.printf "\n"

let compare_nodes n1 n2 = ((n1.agent = n2.agent) && (n1.site = n2.site))

let compare_edges e1 e2 = match (e1,e2) with
  | (Bound (n1,n2), Bound (n1',n2')) ->
     (compare_nodes n1 n1')&&(compare_nodes n2 n2')
  | (Free n1, Free n2) -> (compare_nodes n1 n2)
  | (Bound _, Free _) | (Free _, Bound _) -> false

let compare_internal_states s1 s2 =
  (compare_nodes s1.agent_site s2.agent_site)&&
    (s1.property=s2.property)

let check_site_graph graph =
  let check_site a_type s = true in
  let conflict_free = function
    | (Bound (n1,n2), Bound (n1',n2')) ->
       if ((compare_nodes n1 n2) || (compare_nodes n1' n2') ||
             (compare_nodes n1 n1') || (compare_nodes n1 n2') ||
               (compare_nodes n2 n1') || (compare_nodes n2 n2')) then false
       else true
    | (Bound (n1,n2), Free n) | (Free n, Bound (n1,n2)) ->
       if ((compare_nodes n1 n2) || (compare_nodes n1 n) ||
             (compare_nodes n2 n)) then false
       else true
    | (Free n1, Free n2) -> not (compare_nodes n1 n2) in
  let b1 =
    List.fold_left
      (fun ok n -> ok&&(check_site n.agent_type n.site)) true graph.nodes in
  let rec verify_edges ok = function
    | e1::l ->
       verify_edges
         (List.fold_left
            (fun ok' e2 -> ok'&&(conflict_free (e1,e2))) ok l) l
    | [] -> true in
  let rec verify_internal_states ok = function
    | is1::l ->
       not (List.exists (fun is2 -> compare_internal_states is1 is2) l)
    | [] -> true in
  b1&&(verify_edges true graph.edges)&&
    (verify_internal_states true graph.property_set)

let pushout graph1 graph2 =
  let graph =
    {
      nodes = Lib.remove_duplicates (List.append graph1.nodes graph2.nodes)
                                    compare_nodes;
      edges = Lib.remove_duplicates (List.append graph1.edges graph2.edges)
                                    compare_edges;
      property_set = Lib.remove_duplicates
                       (List.append graph1.property_set graph2.property_set)
                       compare_internal_states;
    } in
  if (check_site_graph graph) then (Some graph)
  else None

let test () =
  print_graph test_graph1;
  if (check_site_graph test_graph1) then Format.printf "site graphs\n"
  else Format.printf "not a site graphs\n";
  print_graph test_graph2;
  if (check_site_graph test_graph1) then Format.printf "site graphs\n"
  else Format.printf "not a site graphs\n"
