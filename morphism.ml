
module IntMap = Map.Make(struct type t = int let compare = compare end)

let empty_morph = IntMap.empty

let poset_minus p rem_events =
  let events =
    List.filter (fun e -> (not (List.mem e rem_events))) p.Poset.events in
  let ids = List.map (fun e -> e.Poset.event_id) rem_events in
  let filter_pair_list ls =
    List.filter
      (fun (e_1,e_2) ->
        (not ((List.mem e_1 ids) || (List.mem e_2 ids)))) ls in
  { Poset.events = events;
    Poset.prec_1 = (filter_pair_list p.Poset.prec_1);
    Poset.inhibit = (filter_pair_list p.Poset.inhibit); }

(*
let ok_pair id1 id2 p1 p2 morph =
  let e1 = List.find (fun e -> e.event_id = id1) p1.events in
  let e2 = List.find (fun e -> e.event_id = id2) p2.events in
  if ((event_compare e1 e2) = 0) then
    List.fold_left
      (fun ok_so_far (i1,i1') ->
        if (ok_so_far && (i1' = id1)) then
          let i2 = IntMap.find i1 morph in
          List.mem (i2,id2) p2.prec_1
        else false) true p1.prec_1
  else false

let rec find_pair e1 l2 p1 p2 morph =
  match l2 with
  | e2::rest2 -> if (ok_pair e1 e2 p1 p2 morph) then (true,rest2,e2)
                 else find_pair e1 rest2 p1 p2 morph
  | [] -> (false,[],e1)

let select_morph l1 l2 p1 p2 old_morph =
  let rec aux_compute_morphism l1 l2 past2 =
    match l1 with
    | e1::rest1 ->
       let (success, rest2, e2) = find_pair e1 l2 p1 p2 old_morph in
       let morph = aux_compute_morphism rest1 (past2@rest2) [] in
       if (success && (not(IntMap.is_empty morph)))
       then (IntMap.add e1 e2 morph)
       else
         if (rest2 = []) then empty_morph
         else aux_compute_morphism l1 rest2 (e2::past2)
    | [] -> empty_morph in
  IntMap.union
    (fun e1 e2 e2' ->
      failwith "it should not happen")
    (aux_compute_morphism l1 l2 []) old_morph
*)

let print_list_morphisms morphs =
  List.iter
    (fun m ->
      Format.printf "morphism: \n";
      List.iter (fun (e1,e2) ->
                  Format.printf "(%d,%d) " e1 e2)
                (IntMap.bindings m))
    morphs
let gen_all_morphs list1 list2 =
  let rec aux_gen_all_morphs l1 l2 past1 =
    if (past1 = []) then []
    else match (l1,l2) with
         | e1::rest1, e2::rest2 ->
            let morphs_below =
              List.map
                (fun m -> IntMap.add e1 e2 m)
                (aux_gen_all_morphs rest1 rest2 rest1) in
            morphs_below@(aux_gen_all_morphs (rest1@[e1]) l2 rest1)
         | [],[] -> []
         | _ -> failwith "this should not happen" in
  let all_combinations = aux_gen_all_morphs list1 list2 list1 in
  let () = if (!Parameter.debug_mode)
           then (Format.printf "poset: gen_all_morphs \n";
                 print_list_morphisms all_combinations) in
  all_combinations

let check_labels m p1 p2 =
  List.fold_left
    (fun ok_so_far (id1,id2) ->
      let e1 = List.find (fun e -> e.Poset.event_id = id1) p1.Poset.events in
      let e2 = List.find (fun e -> e.Poset.event_id = id2) p2.Poset.events in
      ok_so_far && (e1.Poset.event_label = e2.Poset.event_label))
    true (IntMap.bindings m)

let check_prec m p1 p2 =
  List.fold_left
    (fun ok_so_far (id1,id2) ->
      List.fold_left
        (fun ok_so_far' (i1,i1') ->
          if (ok_so_far' && (i1' = id1)) then
            let i2 = IntMap.find i1 m in
            List.mem (i2,id2) p2.Poset.prec_1
          else false) true p1.Poset.prec_1)
    true (IntMap.bindings m)
(*
let rec compute_morphism p1 p2 prev_p1 prev_p2 all_morphs =
  let intro1 = Poset.get_events_from_poset(Poset.intro p1) in
  let intro2 = Poset.get_events_from_poset(Poset.intro p2) in

  if ((List.length intro1) = (List.length intro2)) then
    let all_morphs_this_level =
      gen_all_morphs (List.map (fun e -> e.Poset.event_id) intro1)
                     (List.map (fun e -> e.Poset.event_id) intro2) in
    let valid_labels =
      List.filter (fun m -> check_labels m p1 p2) all_morphs_this_level in
    let morphs =
      if (all_morphs = []) then valid_labels
      else
        let combine_all =
          List.fold_left
            (fun all morph ->
              let aux =
                List.fold_left
                  (fun all_level morph_level ->
                    (IntMap.union
                       (fun e1 e2 e2' -> failwith "it should not happen")
                       morph_level morph)::all_level) [] valid_labels in
              aux@all) [] all_morphs in
        List.filter (fun m -> check_prec m prev_p1 prev_p2) combine_all in
    let () = if (!Parameter.debug_mode)
             then (Format.printf "morphisms at this step: \n";
                   print_list_morphisms morphs) in
    if (morphs = []) then []
    else
      let rest1 = poset_minus p1 intro1 in
      let rest2 = poset_minus p2 intro2 in
      compute_morphism rest1 rest2 p1 p2 morphs
  else []
 *)


let comp_morph p1 p2 =
  if ((List.length p1.Poset.events) <= (List.length p2.Poset.events)) then
    let all_morphs =
      gen_all_morphs (List.map (fun e -> e.Poset.event_id) p1.Poset.events)
                     (List.map (fun e -> e.Poset.event_id) p2.Poset.events) in
    let valid_labels =
      List.filter (fun m -> check_labels m p1 p2) all_morphs in
    let () = if (!Parameter.debug_mode)
             then (Format.printf "morphisms after label check: \n";
                   print_list_morphisms valid_labels) in
    let valid_prec =
      List.filter (fun m -> check_prec m p1 p2) valid_labels in
    let () = if (!Parameter.debug_mode)
             then (Format.printf "morphisms after prec check: \n";
                   print_list_morphisms valid_prec) in
    valid_prec
  else []

let morphism p1 p2 =
  not((comp_morph p1 p2) = [])

(* m : p1 -> p2*)
let check_rev_morph p2 p1 m =
  check_prec m p2 p1

let isomorphism p1 p2 =
  if ((List.length p2.Poset.events) <= (List.length p1.Poset.events)) then
    let iso =
      List.filter (fun m -> check_rev_morph p2 p1 m) (comp_morph p1 p2) in
    (not (iso = []))
  else false
