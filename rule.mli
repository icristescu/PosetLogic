type lnk_or_int = INT of string
                | LNK of Idgraph.link

type quark = Tested of (int*string*lnk_or_int)
            | TestedMod of ((int*string*lnk_or_int)*(int*string*lnk_or_int))
            | Modified of (int*string*lnk_or_int)

type rule = string*(quark list)*((int*string) list)

type t = INIT of rule
       | OBS of rule
       | RULE of rule

val empty : t
val print_quarks : quark list -> ((int*string) list) -> unit
val print : t -> unit

val get_agent : quark -> int
val get_rule_by_label : String.t -> t list -> t

val get_quarks: t -> quark list
val get_node_map: t -> (int*string) list

val filter_on_port: string -> quark list -> quark list
val filter_on_node: int -> quark list -> quark list
val get_nodes : quark list -> int list
val get_ports : quark list -> string list

val test_il : quark -> int list option
val testmod_il : quark -> int list option
val mod_il : quark -> int list option

val find_replace : int*int -> string*int -> quark list -> quark list
