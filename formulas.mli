
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

type ('a) term = Var of string
               | Fn of string * ('a) term list
               | Const of ('a)

type ('a) fol = R of string * ('a) term list

val holds : (string -> Domain.domain list -> Domain.domain) *
              (string -> Domain.domain list -> bool) *
                (Domain.domain list) ->
            (string -> Domain.domain) -> Domain.domain fol formula -> bool

val interpretation : Domain.t -> Rule.t list ->
                     (string -> Domain.domain list -> Domain.domain) *
                       (string -> Domain.domain list -> bool) *
                         Domain.domain list

val denotations : (string -> Domain.domain list -> Domain.domain) *
                    (string -> Domain.domain list -> bool) *
                      (Domain.domain list) ->
                  Domain.domain fol formula -> (string -> Domain.domain) list

val free_var : 'a fol formula -> string list

val print_fm : string fol formula -> unit

val convert_string_to_domain : string fol formula -> Domain.domain list
                               -> Domain.domain fol formula
