open Lib

type t = {
    lhs: ((int*int) list * Pattern.cc * Pattern.id) list option;
    rhs: ((int*int) list * Pattern.cc * Pattern.id) list option;
    eid: int;
  }

let make lhs rhs eid = {lhs;rhs;eid}

let print_cc sigs cc =
  Pattern.print_cc
    ~new_syntax:true ~sigs ~with_id:true (Format.std_formatter) cc

let print_side sigs = function
  | None -> Format.printf "empty";
  | Some mixt ->
     List.iter
       (fun (m,cc,cc_id) ->
         Pattern.print_cc
           ~new_syntax:true ~sigs ~cc_id ~with_id:true
           (Format.std_formatter) cc;
         Format.printf " && ";
         List.iter (fun (a,b) -> Format.printf "(%d,%d) " a b) m)
       mixt;
     Format.printf "@."

let print t sigs =
  Format.printf "@.transition@."; print_side sigs t.lhs;
  Format.printf " => "; print_side sigs t.rhs
