open Lib
exception InfluenceError of string


let refine_prec (e,e') = e'

let refine_conc (e,e') = e'

let obs p = List.nth p.Poset.events (List.length p.Poset.events -1)


(* for now: the events in p.events needs to be ordered,
with last event the observable. to do: order posets after reading.*)

let refine_poset_forward (p:Poset.poset) =
  let rec traverse_and_refine refined_preds next =
    match next with
    | (_,current)::_ ->
       let rec get_all_preds l1 =
         match l1 with
         | (i,i')::l1' ->
            let (l1'',l2'') = get_all_preds l1' in
            if (i' = current) then (l1'', i::l2'')
            else (l1,l2'')
         | [] -> ([],[]) in
       let (next',all_preds) = get_all_preds next in
       let refined_variants =
         List.map
           (fun i ->
             let refined_pred =
               try
                 (List.find (fun e -> fst(e.Poset.event_id) = i) refined_preds)
               with Not_found ->
                 raise (InfluenceError "poset is not ordered") in
             refine_prec(refined_pred, (Poset.get_event_local_id current p)))
           all_preds in
      let refine_concurrent =
        List.fold_left
          (fun e e' ->
            refine_conc(e,e'))
          (List.hd refined_variants) (List.tl refined_variants) in
      traverse_and_refine (refine_concurrent::refined_preds) next'
    |[] -> refined_preds in
  traverse_and_refine [] p.Poset.prec_1
