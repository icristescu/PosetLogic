
type ('a)formula = False
                 | True
                 | Atom of 'a
                 | Not of ('a)formula
                 | And of ('a)formula * ('a)formula
                 | Or of ('a)formula * ('a)formula
                 | Imp of ('a)formula * ('a)formula
                 | Iff of ('a)formula * ('a)formula
                 | Forall of string * ('a)formula
                 | Exists of string * ('a)formula;;

type term = Var of string
          | Fn of string * term list

type fol = R of string * term list

val eval : 'a formula -> ('a -> bool) -> bool

val holds : (string -> 'b list -> 'b) * (string -> 'b list -> bool) ->
            (string -> 'b) -> fol formula -> bool


val interpretation : unit -> (string -> Poset.domain list -> Poset.domain) *
                                 (string -> Poset.domain list -> bool)
