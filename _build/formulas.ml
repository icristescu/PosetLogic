open Format
open Lib

type ('a)formula = False
                 | True
                 | Atom of 'a
                 | Not of ('a)formula
                 | And of ('a)formula * ('a)formula
                 | Or of ('a)formula * ('a)formula
                 | Imp of ('a)formula * ('a)formula
                 | Iff of ('a)formula * ('a)formula
                 | Forall of string * ('a)formula
                 | Exists of string * ('a)formula

(* for propositional logic : prop formula*)
type prop = P of string

(* v satisfies fm if eval fm v = true *)
let rec eval fm v =
  match fm with
    False -> false
  | True -> true
  | Atom(x) -> v(x)
  | Not(p) -> not(eval p v)
  | And(p,q) -> (eval p v) && (eval q v)
  | Or(p,q) -> (eval p v) || (eval q v)
  | Imp(p,q) -> not(eval p v) || (eval q v)
  | Iff(p,q) -> (eval p v) = (eval q v)
  | _ -> false

type ('a) term =  Var of string
                | Fn of string * ('a) term list
                | Const of ('a)

type ('a) fol = R of string * ('a) term list

let rec string_term = function
  | Var s -> Var s
  | Fn (s,ls) -> Fn (s,(List.map (fun tm -> string_term tm) ls))
  | Const (s:string) -> Const (Poset.Pos Poset.empty_poset)

let rec map_fm f fm =
  match fm with
    Atom a -> Atom (f a)
  | Not(p) -> Not(map_fm f p)
  | And(p,q) -> And(map_fm f p,map_fm f q)
  | Or(p,q) -> Or(map_fm f p,map_fm f q)
  | Imp(p,q) -> Imp(map_fm f p,map_fm f q)
  | Iff(p,q) -> Iff(map_fm f p,map_fm f q)
  | Forall(x,p) -> Forall(x,map_fm f p)
  | Exists(x,p) -> Exists(x,map_fm f p)
  | True -> True
  | False -> False

let convert_string_to_domain (fm: string fol formula) =
  let aux_fun = function
      R(s,ls) -> R(s, (List.map (fun tm -> string_term tm) ls)) in
  map_fm aux_fun fm

let print_var str = printf "%s" str

let rec print_tm = function
  | Var v -> printf " %s " v
  | Fn (fn_name,tmls) -> printf " %s( " fn_name;
                         List.iter (fun tm -> print_tm tm) tmls;
                         printf ")"
  | Const d -> match d with
               | Poset.Ev e -> Poset.print_event e
               | Poset.Pos p -> Poset.print_poset p

let rec print_tm_string = function
  | Var v | Const v -> printf " %s " v
  | Fn (fn_name,tmls) -> printf " %s( " fn_name;
                         List.iter (fun tm -> print_tm_string tm) tmls;
                         printf ")"

let print_atom = function
  | R(str, tmls) -> printf "%s (" str;
                    List.iter (fun tm -> print_tm_string tm) tmls;
                    printf ")"

let rec print_fm = function
  | True -> printf "True"
  | False -> printf "False"
  | Atom a -> print_atom a
  | Not(p) -> printf "Not ("; print_fm p; printf ")"
  | And(p,q) -> printf "And("; print_fm p; printf", "; print_fm q; printf ")"
  | Or(p,q) -> printf "Or("; print_fm p; printf", "; print_fm q; printf ")"
  | Imp(p,q) -> printf "Imp("; print_fm p; printf", "; print_fm q; printf ")"
  | Iff(p,q) -> printf "iff("; print_fm p; printf", "; print_fm q; printf ")"
  | Forall(x,p) -> printf "Forall %s." x; printf "("; print_fm p; printf ")"
  | Exists(x,p) -> printf "Exists %s." x; printf "("; print_fm p; printf ")"

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
  | Forall(x,p) | Exists(x,p) -> List.filter (fun y -> not(x=y)) (fv p)

let free_var fm = remove_duplicates(fv fm)

let add_valuation (x,a) v y = if (x=y) then a else v(y)

let trivial_val domain x = domain

let rec forall p l =
  match l with
    [] -> true
  | h::t -> p(h) && forall p t

let rec exists p l =
  match l with
    [] -> false
  | h::t -> p(h) || exists p t

(*
 define the meaning of a term or formula with respect to both an interpretation,
 which specifies the interpretation of the function and predicate symbols,
 and a valuation which specifies the meanings of variables.
*)

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
  | And(p,q) -> (holds m v p) && (holds m v q)
  | Or(p,q) -> (holds m v p) || (holds m v q)
  | Imp(p,q) -> not(holds m v p) || (holds m v q)
  | Iff(p,q) -> (holds m v p = holds m v q)
  | Forall(x,p) -> forall (fun a -> holds m (add_valuation (x,a) v) p) domain
  | Exists(x,p) -> exists (fun a -> holds m (add_valuation (x,a) v) p) domain

let rec denotations (func,pred,domain as m) fm =
  let x = List.hd (fv fm) in
  let valuation p y =
    if (x=y) then p else failwith "uninterpreted variable" in
  List.fold_left
    (fun valid p ->
      if (holds m (valuation p) fm) then p::valid else valid) [] domain
(*
let den free_var  =
  match free_var with
  | x::rest ->
     List.fold_left
       (fun v p -> add_valuation (x1,p) v) empty_val domain
 *)
(**** Interpretation ****)

let label x = match x with
  | Poset.Ev e -> e.Poset.event_label
  | Poset.Pos _ -> failwith "wrong arguments for label"

let cause = function
  | (Poset.Ev e, Poset.Ev e', Poset.Pos p) -> true
  | _ -> failwith "wrong arguments for cause"

let id_eq x y = (x = y)

let membership = function
    (Poset.Ev e, Poset.Pos p) -> List.mem e (p.Poset.events)
  | _ -> failwith "wrong arguments for cause"

let equal_posets = function
    (Poset.Pos p1, Poset.Pos p2) -> Morphism.isomorphism p1 p2
  | _ -> failwith "wrong arguments for equal posets"

let sub_poset = function
    (Poset.Pos p1, Poset.Pos p2) -> Morphism.morphism p1 p2
  | _ -> failwith "wrong arguments for equal posets"

let intro = function
    Poset.Pos p -> Poset.intro p
  | _ -> failwith "wrong arguments for intro"

let obs = function
    Poset.Pos p -> p
  | _ -> failwith "wrong arguments for obs"

let id_label_event str = function
    [e] -> ((label e) = str)
  | _ -> failwith "wrong arguments for labels"

let check_pred p =
  if ((String.length p) >= 5) then
    let is_label = String.sub p 0 4 in (*label_A*)
    (is_label = "label")
  else false

let interpretation t =
  let domain = List.map (fun p -> Poset.Pos(p)) (Poset.get_posets(t)) in
  let func f args =
    match (f,args) with
    | ("intro", [p]) -> Poset.Pos (intro p)
    | ("obs", [p1]) -> p1
    | ("union", [p1;p2]) -> p1
    | ("intersection", [p1;p2]) -> p1
    | _ -> failwith "uninterpreted function" in
  let pred p args =
    if (check_pred p) then
      id_label_event (String.sub p 6 (String.length p)) args
    else
      match (p,args) with
        ("cause",[x;y;p]) -> cause (x,y,p)
      | ("equal_event_labels",[x; y]) -> id_eq (label x) (label y)
      | ("in", [x;p]) -> membership (x,p)
      | ("equal_posets", [p1;p2]) -> equal_posets (p1,p2)
      | ("sub_posets", [p1;p2]) -> sub_poset (p1,p2)
      | ("negative_influence", [x1;p1;x2;p2]) -> true
      | _ -> failwith "uninterpreted predicate" in
  (func,pred,domain)
