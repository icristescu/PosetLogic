
module IntMap = Map.Make(struct type t = int let compare = compare end)

let empty_morph = IntMap.empty

let print_list_morphisms morphs =
  List.iter
    (fun m ->
      Format.printf "morphism: ";
      List.iter (fun (e1,e2) ->
                  Format.printf "(%d,%d) " e1 e2)
                (IntMap.bindings m);
      Format.printf "\n")
    morphs

let gen_one_morphism p1 p2 =
  let l1 = List.map (fun (id,_) -> id) p1 in
  let l2 = List.map (fun (id,_) -> id) p2 in
  List.fold_left2
    (fun m e1 e2 -> IntMap.add e1 e2 m)
    IntMap.empty l1 l2

(* let () = Format.printf "permutations@." in*)
let rm x l = List.filter ((<>) x) l

let check (_,x1) (_,x2) = (x1 = x2)

let rec permutations l1 l2 = match (l1,l2) with
  | ([], []) -> []
  | (x1::[], x2::[]) -> if (check x1 x2 ) then [[x1]] else [[]]
  | (l, x2::l2') ->
    List.fold_left
      (fun acc x ->
        if (check x x2) then
          (let perms = permutations (rm x l) l2' in
           if (perms = [[]]) then acc
           else acc @ List.map (fun p -> x::p) (perms))
        else acc) [] l
  | _ -> raise (ExceptDefn.Internal_Error("permutations arguments"))

let gen_morphisms_permutations_l1 l1 l2 =
  let permuts = permutations l1 l2 in
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
  aux k [] emit list

let check_combination p1 p2 =
  List.for_all
    (fun (_,e1) ->
      List.exists (fun (_,e2) -> (e1 = e2)) p2)
    p1

let gen_all_morphs p1 p2 =
  let l1 =
    List.map (fun e -> (Event.id e, Event.label e)) p1.Poset.events in
  let l2 =
    List.map (fun e -> (Event.id e, Event.label e)) p2.Poset.events in
  let combins = combinations (List.length l1) l2 in
  List.flatten
    (List.map
       (fun short_l2 -> if (check_combination l1 short_l2) then
                          gen_morphisms_permutations_l1 l1 short_l2
                        else [])
       combins)

let check_labels m p1 p2 =
  List.fold_left
    (fun ok_so_far (id1,id2) ->
      let e1 = List.find (fun e -> (Event.id e) = id1) p1.Poset.events in
      let e2 = List.find (fun e -> (Event.id e) = id2) p2.Poset.events in
      ok_so_far && ((Event.label e1)= (Event.label e2)))
    true (IntMap.bindings m)

let check_prec m p1 p2 =
  List.fold_left
    (fun ok (e1,e1') ->
      let e2 = IntMap.find e1 m in
      let e2' = IntMap.find e1' m in
      ok && (List.mem (e2,e2') p2.Poset.prec_1))
    true p1.Poset.prec_1

let comp_morph p1 p2 =
  let all_morphs = gen_all_morphs p1 p2 in
  let () = if (!Param.debug_mode)
           then (Format.printf "gen all morphisms\n";
           print_list_morphisms all_morphs) in
  let valid_prec =
    List.filter (fun m -> check_prec m p1 p2) all_morphs in
  let () = if (!Param.debug_mode)
           then (Format.printf "morphisms after prec check: \n";
                 print_list_morphisms valid_prec) in
  valid_prec

(* m : p1 -> p2*)
let check_rev_morph p2 p1 m =
  check_prec m p2 p1

let isomorph p1 p2 =
  let iso = List.filter (fun m -> check_rev_morph p2 p1 m) (comp_morph p1 p2) in
  let () = if (!Param.debug_mode)
           then (Format.printf "isomorphisms: \n"; print_list_morphisms iso) in
  (not (iso = []))

let morphism p1 p2 =
  if ((List.length p1.Poset.events) = (List.length p2.Poset.events)) then
    isomorph p1 p2
  else
    if ((List.length p1.Poset.events) <= (List.length p2.Poset.events)) then
         not((comp_morph p1 p2) = [])
    else false

let isomorphism p1 p2 =
  if ((List.length p1.Poset.events) = (List.length p2.Poset.events))
  then isomorph p1 p2 else false
