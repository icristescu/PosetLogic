open Lib

type t = {
    lhs: Pattern.cc list;
    rhs: Pattern.cc list;
  }

type s = {
    lhs_state: Replay.state;
    rhs_state: Replay.state;
  }

let empty sigs = {lhs=[Pattern.empty_cc sigs];rhs=[Pattern.empty_cc sigs];}

let print t sigs =
  Format.printf "\ntransition\n";
  List.iter (fun cc ->
              Pattern.print_cc ~new_syntax:true ~sigs:sigs
                               (Format.std_formatter) cc ) t.lhs;
  Format.printf " => ";
  List.iter (fun cc ->
              Pattern.print_cc ~new_syntax:true ~sigs:sigs
                               (Format.std_formatter) cc) t.rhs

let pattern_transition contact_map sigs pre_env s =
  let lhs_state = s.lhs_state in
  let (_,lhs) =
    Snip.patterns_of_mixture contact_map sigs pre_env
                             (lhs_state.Replay.graph) in
  let rhs_state = s.rhs_state in
  let (_,rhs) =
    Snip.patterns_of_mixture contact_map sigs pre_env
                             (rhs_state.Replay.graph) in
  {lhs;rhs}

let get_rhs_state s = s.rhs_state
let make lhs_state rhs_state = {lhs_state;rhs_state}
