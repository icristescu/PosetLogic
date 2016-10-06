
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

let print_list_morphisms morphs =
  List.iter
    (fun m ->
      Format.printf "morphism: ";
      List.iter (fun (e1,e2) ->
                  Format.printf "(%d,%d) " e1 e2)
                (IntMap.bindings m);
      Format.printf "\n")
    morphs

let gen_one_morphism l1 l2 =
  List.fold_left2
    (fun m e1 e2 -> IntMap.add e1 e2 m)
    IntMap.empty l1 l2

let ins_all_positions x l =
  let rec aux prev acc = function
    | [] -> (prev @ [x]) :: acc |> List.rev
    | hd::tl as l -> aux (prev @ [hd]) ((prev @ [x] @ l) :: acc) tl
  in
  aux [] [] l

let rec permutations = function
  | [] -> []
  | x::[] -> [[x]]
  | x::xs ->
     List.fold_left
       (fun acc p -> acc @ ins_all_positions x p ) [] (permutations xs)

let rec gen_morphisms_permutations_l1 l1 l2 =
  let permuts = permutations l1 in
  List.map
    (fun l1' -> gen_one_morphism l1' l2)
    permuts

let combinations k list =
    let rec aux k acc emit = function
      | [] -> acc
      | h :: t ->
        if k = 1 then aux k (emit [h] acc) emit t else
          let new_emit x = emit (h :: x) in
          aux k (aux (k-1) acc new_emit t) emit t
    in
    let emit x acc = x :: acc in
    aux k [] emit list;;

let gen_all_morphs l1 l2 =
  let combins = combinations (List.length l1) l2 in
  List.flatten
    (List.map
       (fun short_l2 -> gen_morphisms_permutations_l1 l1 short_l2)
       combins)

let check_labels m p1 p2 =
  List.fold_left
    (fun ok_so_far (id1,id2) ->
      let e1 = List.find (fun e -> e.Poset.event_id = id1) p1.Poset.events in
      let e2 = List.find (fun e -> e.Poset.event_id = id2) p2.Poset.events in
      ok_so_far && (e1.Poset.event_label = e2.Poset.event_label))
    true (IntMap.bindings m)

let check_prec m p1 p2 =
  List.fold_left
    (fun ok (e1,e1') ->
      let e2 = IntMap.find e1 m in
      let e2' = IntMap.find e1' m in
      ok && (List.mem (e2,e2') p2.Poset.prec_1))
    true p1.Poset.prec_1

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
    let () = if (!Parameter.debug_mode)
             then (Format.printf "isomorphisms: \n";
                   print_list_morphisms iso) in
    (not (iso = []))
  else false
