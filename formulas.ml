open Lib

type ('a)formula = False
                 | True
                 | Atom of 'a
                 | Not of ('a)formula
                 | And of ('a)formula * ('a)formula
                 | Or of ('a)formula * ('a)formula
                 | Imp of ('a)formula * ('a)formula
                 | Iff of ('a)formula * ('a)formula
                 | Forall of string * string * ('a)formula
                 | Exists of string * string * ('a)formula

type ('a) term =  Var of string
                | Fn of string * ('a) term list
                | Const of ('a)

type ('a) fol = R of string * ('a) term list

let rec string_term tm domain = match tm with
  | Var s -> Var s
  | Fn (s,ls) -> Fn (s,(List.map (fun tm -> string_term tm domain) ls))
  | Const s -> Const (Domain.get_poset_from_filename s domain)

let rec map_fm f fm =
  match fm with
    Atom a -> Atom (f a)
  | Not(p) -> Not(map_fm f p)
  | And(p,q) -> And(map_fm f p,map_fm f q)
  | Or(p,q) -> Or(map_fm f p,map_fm f q)
  | Imp(p,q) -> Imp(map_fm f p,map_fm f q)
  | Iff(p,q) -> Iff(map_fm f p,map_fm f q)
  | Forall(x,s,p) -> Forall(x,s,map_fm f p)
  | Exists(x,s,p) -> Exists(x,s,map_fm f p)
  | True -> True
  | False -> False

let convert_string_to_domain (fm: string fol formula) domain =
  let aux_fun = function
      R(s,ls) -> R(s, (List.map (fun tm -> string_term tm domain) ls)) in
  map_fm aux_fun fm

(* Printing *)

let print_var str = Format.printf "%s" str

let rec print_tm = function
  | Var v -> Format.printf " %s " v
  | Fn (fn_name,tmls) -> Format.printf " %s( " fn_name;
                         List.iter (fun tm -> print_tm tm) tmls;
                         Format.printf ")"
  | Const d -> match d with
               | Domain.Ev e -> Event.print_event e
               | Domain.Pos p -> Poset.print_poset p

let rec print_tm_string = function
  | Var v | Const v -> Format.printf " %s " v
  | Fn (fn_name,tmls) -> Format.printf " %s( " fn_name;
                         List.iter (fun tm -> print_tm_string tm) tmls;
                         Format.printf ")"

let print_atom = function
  | R(str, tmls) -> Format.printf "%s (" str;
                    List.iter (fun tm -> print_tm_string tm) tmls;
                    Format.printf ")"

let rec print_fm = function
  | True -> Format.printf "true"
  | False -> Format.printf "false"
  | Atom a -> print_atom a
  | Not(p) -> Format.printf "not ("; print_fm p; Format.printf ")"
  | And(p,q) -> Format.printf "and("; print_fm p; Format.printf", "; print_fm q;
                Format.printf ")"
  | Or(p,q) -> Format.printf "or("; print_fm p; Format.printf", "; print_fm q;
               Format.printf ")"
  | Imp(p,q) -> Format.printf "imp("; print_fm p; Format.printf", "; print_fm q;
                Format.printf ")"
  | Iff(p,q) -> Format.printf "iff("; print_fm p; Format.printf", "; print_fm q;
                Format.printf ")"
  | Forall(x,s,p) -> Format.printf"forall %s : %s." x s;Format.printf"(";
                     print_fm p;Format.printf")"
  | Exists(x,s,p) -> Format.printf"exists %s : %s." x s;Format.printf"(";
                     print_fm p;Format.printf")"

(* Free variables in terms and formulas. *)

let rec fvt tm =
  match tm with
    Var x -> [x]
  | Const c -> []
  | Fn(f,args) -> List.flatten (List.map fvt args)

let rec fv fm =
  match fm with
  | False | True -> []
  | Atom(R(p,args)) ->  List.flatten (List.map fvt args)
  | Not(p) -> fv p
  | And(p,q) | Or(p,q) | Imp(p,q) | Iff(p,q) -> (fv p)@(fv q)
  | Forall(x,_,p) | Exists(x,_,p) -> List.filter (fun y -> not(x=y)) (fv p)

let free_var fm = remove_duplicates (fv fm) (fun i1 i2 -> i1 = i2)

(*
 define the meaning of a term or formula with respect to both an interpretation,
 which specifies the interpretation of the function and predicate symbols,
 and a valuation which specifies the meanings of variables.
*)


let add_valuation (x,a) v y =  if (x=y) then a else v(y)

let print_valuation valuation vars =
  List.iter
    (fun var ->
      match (valuation var) with
      | Domain.Pos p -> Poset.print_poset p
      | Domain.Ev e -> Event.print_event e) vars

let domain_match_sort d s = match (d,s) with
  | (Domain.Pos _, "Poset") -> true
  | (Domain.Ev _, "Event") -> true
  | _ -> false

let trivial_val domain x = domain

let rec forall p l =
  match l with
    [] -> true
  | h::t -> p(h) && forall p t

let rec exists p l =
  match l with
    [] -> false
  | h::t -> p(h) || exists p t

let rec termval func v tm =
  match tm with
    Var(x) -> v(x)
  | Const(c) -> c
  | Fn(f,args) -> func f (List.map (termval func v) args)

let rec holds (func,pred,domain as m) v fm =
  match fm with
    False -> false
  | True -> true
  | Atom(R(r,args)) -> pred r (List.map (termval func v) args)
  | Not(p) -> not(holds m v p)
  | And(p,q) -> ((holds m v p) && (holds m v q))
  | Or(p,q) -> (holds m v p) || (holds m v q)
  | Imp(p,q) -> not(holds m v p) || (holds m v q)
  | Iff(p,q) -> (holds m v p = holds m v q)
  | Forall(x,s,p) ->
     forall (fun a -> holds m (add_valuation (x,a) v) p)
            (List.filter (fun d -> domain_match_sort d s) domain)
  | Exists(x,s,p) ->
     exists (fun a -> holds m (add_valuation (x,a) v) p)
            (List.filter (fun d -> domain_match_sort d s) domain)

let denotations (_,_,domain as m) fm =
  let rec denote_var vars valuation vals =
    match vars with
    | x::next ->
       let () = if (!Param.debug_mode) then
                  Format.printf "denotations for one var %s\n" x in
       List.fold_left
         (fun l p -> denote_var next (add_valuation (x,p) valuation) l)
         vals
         (List.filter (fun d -> domain_match_sort d "Poset") domain)
    | [] ->
       if (holds m valuation fm) then (valuation::vals) else (vals) in
  let default_valuation x = raise (ExceptDefn.Uninterpreted_Variable(x)) in
  denote_var (free_var fm) default_valuation []

(**** Interpretation ****)

let label x = match x with
  | Domain.Ev e -> (Event.get_label e)
  | Domain.Pos _ ->
     raise (ExceptDefn.Malformed_Args("label"))

let prec_1 = function
  | (Domain.Ev e, Domain.Ev e', Domain.Pos p) ->
     let () = if (!Param.debug_mode) then
                Format.printf "prec_1 " in
     Poset.check_prec_1 e e' p
  | _ -> raise (ExceptDefn.Malformed_Args("prec_1"))

let prec_star = function
  | (Domain.Ev e, Domain.Ev e', Domain.Pos p) ->
     let () = if (!Param.debug_mode) then
                (Format.printf "prec_star: ";
                 Event.print_event e; Event.print_event e';
                 Poset.print_poset p) in
     Poset.check_prec_star e e' p
  | _ -> raise (ExceptDefn.Malformed_Args("prec_star"))

let id_eq x y =
  let () = if (!Param.debug_mode) then
               Format.printf "id_eq %s = %s\n" x y in
  (x = y)

let membership = function
    (Domain.Ev e, Domain.Pos p) -> List.mem e (p.Poset.events)
  | _ -> raise (ExceptDefn.Malformed_Args("membership"))

let equal_posets = function
    (Domain.Pos p1, Domain.Pos p2) ->
    (match (p1.Poset.kappa, p2.Poset.kappa) with
     | (true, true) ->
        Morphism.isomorphism (Poset.remove_obs(p1)) (Poset.remove_obs(p2))
     | (true, false) -> Morphism.isomorphism (Poset.remove_obs(p1)) p2
     | (false, true) -> Morphism.isomorphism p1 (Poset.remove_obs(p2))
     | (false, false) -> Morphism.isomorphism p1 p2 )
  | _ -> raise(ExceptDefn.Malformed_Args("equal posets"))

let equal_events = function
    (Domain.Ev e1, Domain.Ev e2) ->
    let () = if (!Param.debug_mode) then
               (Format.printf "equal_events"; Event.print_event e1;
                Event.print_event e1) in
    if (e1 = e2) then true
    else false
  | _ -> raise(ExceptDefn.Malformed_Args("equal events"))

let sub_poset = function
    (Domain.Pos p1, Domain.Pos p2) ->
    (match (p1.Poset.kappa, p2.Poset.kappa) with
     | (true, true) ->
        let () = if (!Param.debug_mode) then
                   (Format.printf
                      "sub_poset: posets are kappa type - remove obs\n";) in
        Morphism.morphism (Poset.remove_obs(p1)) (Poset.remove_obs(p2))
     | (true, false) -> Morphism.morphism (Poset.remove_obs(p1)) p2
     | (false, true) -> Morphism.morphism p1 (Poset.remove_obs(p2))
     | (false, false) -> Morphism.morphism p1 p2 )
  | _ -> raise(ExceptDefn.Malformed_Args("sub_posets"))

let intro = function
    Domain.Pos p -> Poset.intro p
  | _ -> raise (ExceptDefn.Malformed_Args("intro"))

let obs = function
    Domain.Pos p -> p
  | _ -> raise (ExceptDefn.Malformed_Args("obs"))

let negative_influence = function
    (Domain.Pos p1, Domain.Pos p2) ->
    let t = Concret.concret p1 in
    let () = if (!Param.debug_mode) then TraceConcret.print t in
    true
   | _ ->  raise(ExceptDefn.Malformed_Args("negative_influence"))

let id_label_event str = function
    [e] -> ((label e) = str)
  | _ -> raise (ExceptDefn.Malformed_Args("id_label_event"))

let check_pred p =
  if ((String.length p) >= 5) then
    let is_label = String.sub p 0 5 in
    (String.equal is_label "label")
  else false

let interpretation t =
  let domain = (List.map (fun p -> Domain.Pos(p)) (Domain.get_posets(t)))@
                 (List.map (fun e -> Domain.Ev(e)) (Domain.get_events(t))) in
  let func f args =
    match (f,args) with
    | ("intro", [p]) -> Domain.Pos (intro p)
    | ("obs", [p1]) -> p1
    | ("union", [p1;p2]) -> p1
    | ("intersection", [p1;p2]) -> p1
    | _ -> raise (ExceptDefn.Uninterpreted_Function(f)) in
  let pred p args =
    if (check_pred p) then
      let lb = String.sub p 5 ((String.length p) - 5) in
      id_label_event lb args
    else
      match (p,args) with
        ("prec_1",[x;y;p]) -> prec_1 (x,y,p)
      | ("prec_star",[x;y;p]) -> prec_star (x,y,p)
      | ("equal_event_labels",[x; y]) -> id_eq (label x) (label y)
      | ("in", [x;p]) -> membership (x,p)
      | ("equal_posets", [p1;p2]) -> equal_posets (p1,p2)
      | ("equal_events", [e1;e2]) -> equal_events (e1,e2)
      | ("sub_posets", [p1;p2]) -> sub_poset (p1,p2)
      | ("negative_influence", [p1;p2]) ->
         negative_influence (p1,p2)
      | _ -> raise (ExceptDefn.Uninterpreted_Predicate(p)) in
  (func,pred,domain)
