
type kt = INIT of Site_graph.t
        | OBS of string*Site_graph.t
        | RULE of string*Site_graph.t*Site_graph.t

type t = {
    left : Site_graph.t;
    right : Site_graph.t;
    middle : Site_graph.t;
    id : int;
  }

let print rule = Format.printf "\n"
