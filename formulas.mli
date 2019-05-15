
type 'a formula = False
                 | True
                 | Atom of 'a
                 | Not of 'a formula
                 | And of 'a formula * 'a formula
                 | Or of 'a formula * 'a formula
                 | Imp of 'a formula * 'a formula
                 | Iff of 'a formula * 'a formula
                 | Forall of string * string * 'a formula
                 | Exists of string * string * 'a formula

type 'a term = Var of string
               | Fn of string * 'a term list
               | Const of 'a

type 'a fol = R of string * 'a term list

(** define the meaning of a term or formula with respect to both
    an interpretation (of the function and predicate symbols) and
    a valuation (of the free variables). *)
val holds : (string -> Domain.domain list -> Domain.domain) *
              (string -> Domain.domain list -> bool) *
                (Domain.domain list) ->
            (string -> Domain.domain) -> Domain.domain fol formula -> bool

val interpretation : Model.t -> Domain.t ->
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

val replace_variables : 'a fol formula list -> (string * 'a) list ->
                        'a fol formula list
