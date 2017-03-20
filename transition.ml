open Lib

type t = {
    lhs: Pattern.cc list;
    rhs: Pattern.cc list;
    eid: int;
  }

type s = {
    lhs_state: Replay.state;
    rhs_state: Replay.state;
    eid_state: int;
  }

let empty sigs =
  {lhs=[Pattern.empty_cc sigs];rhs=[Pattern.empty_cc sigs];eid=(-1)}

let copy_state s = {
    Replay.graph = (Edges.copy s.Replay.graph); time=s.Replay.time;
    event = s.Replay.event;connected_components=s.Replay.connected_components;}

let copy s =
  {lhs_state = (copy_state s.lhs_state);
   rhs_state = (copy_state s.rhs_state);
   eid_state = s.eid_state}

let print_side s sigs =
  List.iter (fun cc -> Pattern.print_cc ~new_syntax:true ~sigs:sigs
                                        ~with_id:true
                                        (Format.std_formatter) cc ) s;
  Format.printf "\n"

let print t sigs =
  Format.printf "\ntransition\n"; print_side t.lhs sigs;
  Format.printf " => "; print_side t.rhs sigs

let pattern_transition sigs contact_map s =
  let pre_env = Pattern.PreEnv.empty sigs in
  let lhs_state = s.lhs_state in
  let (_,lhs) =
    Snip.patterns_of_mixture contact_map sigs pre_env
                             (lhs_state.Replay.graph) in
  let pre_env = Pattern.PreEnv.empty sigs in
  let rhs_state = s.rhs_state in
  let (_,rhs) =
    Snip.patterns_of_mixture contact_map sigs pre_env
                             (rhs_state.Replay.graph) in
  {lhs;rhs;eid=(s.eid_state)}

let get_rhs_state s = s.rhs_state
let make lhs_state rhs_state eid_state = {lhs_state;rhs_state;eid_state}
