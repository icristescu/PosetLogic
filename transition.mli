type t = {
    lhs: Idgraph.mixture;
    rhs: Idgraph.mixture;
    rule : Ast.t;
  }

val empty : t
val print : t -> unit
val get_rhs : t -> Idgraph.mixture
val make : Idgraph.mixture -> Ast.t -> Event.quark list -> t
