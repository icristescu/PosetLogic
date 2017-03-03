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

type t = {
    nodes : node list;
    edges : edge list;
    property_set : internal_state list;
  }

let empty = {nodes=[];edges=[];property_set=[]}

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

let test_graph3 =
  let node0 = {
      agent = 0;
      agent_type = 0;
      site = 0; } in
  let node1 = {
      agent = 1;
      agent_type = 1;
      site = 0; } in
  let node2 = {
      agent = 1;
      agent_type = 1;
      site = 1; } in
  let node3 = {
      agent = 2;
      agent_type = 0;
      site = 0; } in
  let edge0 = Bound (node0, node1) in
  let edge1 = Bound (node2, node3) in
  { nodes = [node0; node1; node2; node3];
    edges = [edge0; edge1]; property_set = [] }

let test_graph4 =
  let node0 = {
      agent = 3;
      agent_type = 2;
      site = 0; } in
  let node1 = {
      agent = 2;
      agent_type = 0;
      site = 1; } in
  let edge0 = Bound (node0, node1) in
  { nodes = [node0; node1]; edges = [edge0]; property_set = [] }

let test_graph5 =
  let node0 = {
      agent = 0;
      agent_type = 0;
      site = 0; } in
  let node1 = {
      agent = 0;
      agent_type = 0;
      site = 1; } in
  let edge = Bound (node0, node1) in
  { nodes = [node0; node1]; edges = [edge]; property_set = [] }

let bad_graph1 =
  let node = {
      agent = 0;
      agent_type = 0;
      site = 0; } in
  let edge = Bound (node, node) in
  { nodes = [node]; edges = [edge]; property_set = [] }

let bad_graph3 =
  let node0 = {
      agent = 0;
      agent_type = 0;
      site = 0; } in
  let node1 = {
      agent = 1;
      agent_type = 0;
      site = 0; } in
  let node2 = {
      agent = 1;
      agent_type = 0;
      site = 1; } in
  let edge0 = Bound (node0, node1) in
  let edge1 = Bound (node0, node2) in
  { nodes = [node0; node1; node2]; edges = [edge0; edge1]; property_set = [] }

let bad_graph4 =
  let node0 = {
      agent = 0;
      agent_type = 0;
      site = 0; } in
  let node1 = {
      agent = 1;
      agent_type = 0;
      site = 0; } in
  let property0 = { agent_site = node0; property = 0} in
  let property2 = { agent_site = node1; property = 0} in
  let property1 = { agent_site = node0; property = 0} in
  { nodes = [node0; node1]; edges = [];
    property_set = [property0; property2; property1]}

let print_node n = Format.printf "(agent,site) = (%d,%d)"
                                 n.agent n.site
let print_edge e = match e with
  | Bound (n1,n2) ->
     Format.printf "["; print_node n1; print_node n2; Format.printf "]\n"
  | Free n1 ->
     Format.printf "["; print_node n1; Format.printf "free]\n"
let print_internal_state i =
    Format.printf "agent_site = "; print_node i.agent_site;
    Format.printf "property = %d " i.property

let print_graph graph =
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
  (*return true if node in edge*)
  let is_node_in_edge n e = match e with
    | Bound (n1,n2) -> (compare_nodes n1 n) || (compare_nodes n2 n)
    | Free n' -> compare_nodes n n' in
  (*return true if edges are in conflict*)
  let conflict_edges e1 e2 = match e1 with
    | Bound (n1,n2) -> (is_node_in_edge n1 e2) || (is_node_in_edge n2 e2)
    | Free n -> is_node_in_edge n e2 in
  let rec verify_edges ok el =
    let () = if (!Parameter.debug_mode) then
               (Format.printf "verify_edges for ";
                List.iter (fun e -> print_edge e) el) in
    match el with
    | e1::l ->
       let not_self_bound = match e1 with
         | Bound (n1,n2) -> not(compare_nodes n1 n2)
         | Free _ -> true in
       verify_edges
         (List.fold_left
            (fun ok' e2 ->
              let ok'' = not(conflict_edges e1 e2) in
              let () = if (!Parameter.debug_mode) then
                         (if ok'' then Format.printf "conflict free edges "
                          else Format.printf "conflict edges ";
                          print_edge e1; print_edge e2) in
              ok'&&ok'') (ok&&not_self_bound) l) l
    | [] -> ok in
  let rec verify_internal_states ok isl =
    let () = if (!Parameter.debug_mode) then
               (Format.printf "\n verify_int_states for ";
                List.iter (fun e -> print_internal_state e) isl) in
    match isl with
    | is1::l ->
       let ok'' =
         not (List.exists
                (fun is2 -> compare_nodes is1.agent_site is2.agent_site) l) in
       if (ok''&&ok) then verify_internal_states true l else false
    | [] -> ok in
  (verify_edges true graph.edges)&&
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

(* let test_check_site_graphs = *)
(*   if (check_site_graph bad_graph1) then Format.printf "bad1 : site graph\n" *)
(*   else Format.printf "bad1 : not a site graphs\n"; *)
(*   if (check_site_graph test_graph5) then Format.printf "graph5 : site graph\n" *)
(*   else Format.printf "graph5 : not a site graphs\n"; *)
(*   if (check_site_graph bad_graph3) then Format.printf "bad3 : site graph\n" *)
(*   else Format.printf "bad3 : not a site graphs\n"; *)
(*   if (check_site_graph bad_graph4) then Format.printf "bad4 : site graph\n" *)
(*   else Format.printf "bad4 : not a site graphs\n"; *)
(*   Format.printf "\n\n" *)

(* let test_pushout = *)
(*   let () = *)
(*     match (pushout test_graph1 test_graph2) with *)
(*     | Some gr -> Format.printf "pushout 1, 2: \n"; print_graph gr *)
(*     | None -> Format.printf "pushout not a site graphs\n" in *)
(*   Format.printf "\n\n"; *)
(*   let () = *)
(*     match (pushout test_graph3 test_graph4) with *)
(*     | Some gr -> Format.printf "pushout 3, 4:\n"; print_graph gr *)
(*     | None -> Format.printf "pushout not a site graphs\n" in *)
(*   () *)

(* let test () = *)
(*   test_pushout *)
