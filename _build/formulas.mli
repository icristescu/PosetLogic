
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

type ('a) term = Var of string
               | Fn of string * ('a) term list
               | Const of ('a)

type ('a) fol = R of string * ('a) term list

val eval : 'a formula -> ('a -> bool) -> bool

val holds : (string -> 'a list -> 'a) * (string -> 'a list -> bool) *
              ('a list) ->
            (string -> 'a) -> 'a fol formula -> bool


val interpretation : Poset.t -> (string -> Poset.domain list -> Poset.domain) *
                                 (string -> Poset.domain list -> bool) *
                                   Poset.domain list

val denotations : (string -> 'a list -> 'a) * (string -> 'a list -> bool) *
                    ('a list) -> 'a fol formula -> 'a list

val free_var : 'a fol formula -> string list

val print_fm : string fol formula -> unit

val convert_string_to_domain : string fol formula -> Poset.domain fol formula
