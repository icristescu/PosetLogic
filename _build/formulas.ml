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

type term = Var of string
          | Fn of string * term list

type fol = R of string * term list

let rec map_fm f fm =
  match fm with
    Atom a -> f a
  | Not(p) -> Not(map_fm f p)
  | And(p,q) -> And(map_fm f p,map_fm f q)
  | Or(p,q) -> Or(map_fm f p,map_fm f q)
  | Imp(p,q) -> Imp(map_fm f p,map_fm f q)
  | Iff(p,q) -> Iff(map_fm f p,map_fm f q)
  | Forall(x,p) -> Forall(x,map_fm f p)
  | Exists(x,p) -> Exists(x,map_fm f p)
  | _ -> fm

(*
 define the meaning of a term or formula with respect to both an interpretation,
 which specifies the interpretation of the function and predicate symbols,
 and a valuation which specifies the meanings of variables.
*)

let rec termval (func,pred as m) v tm =
  match tm with
    Var(x) -> v(x)
  | Fn(f,args) -> func f (List.map (termval m v) args)

let rec holds (_,pred as m) v fm =
  match fm with
    False -> false
  | True -> true
  | Atom(R(r,args)) -> pred r (List.map (termval m v) args)
  | Not(p) -> not(holds m v p)
  | And(p,q) -> (holds m v p) && (holds m v q)
  | Or(p,q) -> (holds m v p) || (holds m v q)
  | Imp(p,q) -> not(holds m v p) || (holds m v q)
  | Iff(p,q) -> (holds m v p = holds m v q)
  | _ -> false
(*  | Forall(x,p) -> forall (fun a -> holds m ((x |-> a) v) p) domain
  | Exists(x,p) -> exists (fun a -> holds m ((x |-> a) v) p) domain;;*)

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
    (Poset.Pos p_1, Poset.Pos p_2) -> Morphism.isomorphism p_1 p_2
  | _ -> failwith "wrong arguments for equal posets"

let sub_poset = function
    (Poset.Pos p_1, Poset.Pos p_2) -> Morphism.morphism p_1 p_2
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

let interpretation () =
  let func f args =
    match (f,args) with
    | ("intro", [p]) -> Poset.Pos (intro p)
    | ("obs", [p1]) -> p1
    | ("union", [p1;p2]) -> p1
    | ("intersection", [p1;p2]) -> p1
    | _ -> failwith "uninterpreted function" in
  let pred p args =
    let is_label = String.sub p 0 4 in (*label_A*)
    if (is_label = "label") then
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
  (func,pred)
