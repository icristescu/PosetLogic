type lnk_or_int = INT of string
                | LNK of Idgraph.link

type quark = Tested of (int*int*lnk_or_int)
            | TestedMod of ((int*int*lnk_or_int)*(int*int*lnk_or_int))
            | Modified of (int*int*lnk_or_int)


type rule = string*(quark list)*Maps.node_map*Maps.port_map

type t = INIT of rule
       | OBS of rule
       | RULE of rule

val empty : t
val print_quarks : quark list -> Maps.node_map -> Maps.port_map -> unit
val print : t -> unit

val get_agent : quark -> int
val get_port : quark -> int
val get_il : quark -> lnk_or_int

val get_rule_by_label : String.t -> t list -> t

val get_quarks: t -> quark list
val get_node_map: t -> Maps.node_map
val get_port_map: t -> Maps.port_map

val filter_on_port: int -> quark list -> quark list
val filter_on_node: int -> quark list -> quark list
val get_nodes : quark list -> int list
val get_ports : quark list -> int list

val test_il : quark -> int list option
val testmod_il : quark -> int list option
val mod_il : quark -> int list option

val find_replace : int*int -> int*int -> quark list -> quark list
val make_agent : string -> string -> quark -> Idgraph.agent
val make_port : int -> lnk_or_int -> string -> Idgraph.port
