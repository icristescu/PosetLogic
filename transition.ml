
type t = {
    lhs: ((int*int) list * Pattern.cc * Pattern.id) list option;
    rhs: ((int*int) list * Pattern.cc * Pattern.id) list;
    eid: int;
  }

let make lhs rhs eid = {lhs;rhs;eid}

let lhs t = t.lhs
let rhs t = t.rhs
let eid t = t.eid

let print t sigs =
  let print_option_side = function
    | None -> Format.printf "empty";
    | Some mixt -> Lib.print_side sigs mixt in
  Format.printf "@.transition@."; print_option_side t.lhs;
  Format.printf " => "; Lib.print_side sigs t.rhs
