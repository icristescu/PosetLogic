let files = ref []
let formula_file = ref ""
let read_fm = ref []
let debug_mode = ref false

let options = [
    ("-f", Arg.Set_string formula_file, "file name skeleton for outputs");
    ("-debug", Arg.Set debug_mode, "print internal info");]

let test_membership t =
  let posets = Domain.get_posets t in
  let events = Domain.get_events t in
  let e1 = List.nth events 4 in
  let p1 = List.hd posets in

  let () = if (!Parameter.debug_mode) then
             ( Format.printf "\n test_membership : event ";
               Event.print_event e1; Format.printf " in poset :\n ";
               Poset.print_poset p1) in

  let valuation x =
    match x with
    | "x" -> Domain.Ev(e1)
    | "y" -> Domain.Pos(p1)
    | _ -> failwith "uninterpreted variable" in
  let fm =
    (Formulas.Atom(Formulas.R("in", [Formulas.Var "x"; Formulas.Var "y"]))) in
  (valuation,fm)

let test_membership_const t =
  let posets = Domain.get_posets t in
  let events = Domain.get_events t in
  let e1 = List.nth events 4 in
  let p1 = List.hd posets in

  let () = if (!Parameter.debug_mode) then
             ( Format.printf "\n test_membership : event ";
               Event.print_event e1; Format.printf " in poset :\n ";
               Poset.print_poset p1) in

  let valuation x =
    match x with
    | "x" -> Domain.Ev(e1)
    | _ -> failwith "uninterpreted variable" in
  let fm =
    (Formulas.Atom(
         Formulas.R(
             "in", [Formulas.Var "x"; Formulas.Const (Domain.Pos(p1))]))) in
  (valuation,fm)

let test_subset t =
  let posets = Domain.get_posets t in
  let p1 = List.nth posets 2 in
  let p2 = List.nth posets 1 in

  let () = if (!Parameter.debug_mode) then
             ( Format.printf "\n test_subset : poset \n";
               Poset.print_poset p1; Format.printf " in poset :\n ";
               Poset.print_poset p2) in

  let valuation x =
    match x with
    | "x" -> Domain.Pos(p1)
    | "y" -> Domain.Pos(p2)
    | _ -> failwith "uninterpreted variable" in
  let fm =
    (Formulas.Atom(
         Formulas.R("sub_posets", [Formulas.Var "x"; Formulas.Var "y"]))) in
  (valuation,fm)

let test_subset_without_obs t =
  let posets = Domain.get_posets t in
  let p1 = Poset.remove_obs(List.nth posets 3) in
  let p2 = Poset.remove_obs(List.nth posets 2) in

  let () = if (!Parameter.debug_mode) then
             ( Format.printf "\n test_subset : poset \n";
               Poset.print_poset p1; Format.printf " in poset :\n ";
               Poset.print_poset p2) in

  let valuation x =
    match x with
    | "x" -> Domain.Pos(p1)
    | "y" -> Domain.Pos(p2)
    | _ -> failwith "uninterpreted variable" in
  let fm =
    (Formulas.Atom(
         Formulas.R("sub_posets", [Formulas.Var "x"; Formulas.Var "y"]))) in
  (valuation,fm)

let test_intro_subset t =
  let posets = Domain.get_posets t in
  let p1 = List.nth posets 2 in
  let valuation x =
    match x with
    | "x" -> Domain.Pos(p1)
    | _ -> failwith "uninterpreted variable" in
  let fm =
    (Formulas.Atom(
         Formulas.R("sub_posets",
                    [Formulas.Fn ("intro", [Formulas.Var "x"]);
                     Formulas.Var "x"]))) in
  (valuation,fm)

let test_forall_intro_subset t =
  let valuation x = failwith "uninterpreted variable" in
  let fm =
    (Formulas.Forall (
         "x", "Poset",
         Formulas.Atom(
             Formulas.R("sub_posets",
                        [Formulas.Fn ("intro", [Formulas.Var "x"]);
                         Formulas.Var "x"])))) in
  (valuation,fm)

let test_denotation_intro_subset t =
  let fm =
    Formulas.Atom(
        Formulas.R("sub_posets",
                   [Formulas.Fn ("intro", [Formulas.Var "x"]);
                    Formulas.Var "x"])) in
  fm

let test_events_labels_in_diff_posets t =
  let posets = Domain.get_posets t in
  let p1 = List.nth posets 1 in
  let e1 = List.nth (Poset.get_events_from_poset(p1)) 1 in
  let p2 = List.nth posets 2 in
  let e2 = List.nth (Poset.get_events_from_poset(p2)) 1 in

  let valuation x =
    match x with
    | "y1" -> Domain.Pos(p1)
    | "x1" -> Domain.Ev(e1)
    | "y2" -> Domain.Pos(p2)
    | "x2" -> Domain.Ev(e2)
    | _ -> failwith "uninterpreted variable" in
  let fm =
    (Formulas.And(
         Formulas.And(
             Formulas.Atom(
                 Formulas.R("in", [Formulas.Var "x1"; Formulas.Var "y1"])),
             Formulas.Atom(
                 Formulas.R("in", [Formulas.Var "x2"; Formulas.Var "y2"]))),
         Formulas.Atom(
             Formulas.R("equal_event_labels",
                        [Formulas.Var "x1"; Formulas.Var "x2"])))) in
  (valuation,fm)

let empty_valuation = function
    _ -> failwith "empty valuation"

let parse_fm () =
  let () = if (!Parameter.debug_mode) then
             Format.printf "parse file %s\n" (!formula_file) in
  let chan = open_in (!formula_file) in
  try
    let lexbuf = Lexing.from_channel chan in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      read_fm := result::(!read_fm);
      if (!Parameter.debug_mode) then
        (Format.printf "parsing \n";Formulas.print_fm result;Format.printf"\n")
    done
  with Lexer.Eof -> ()

let set_flags () =
  Parameter.debug_mode := !debug_mode

let evaluate sfm m v =
  let fm = Formulas.convert_string_to_domain sfm in
  let free_var = Formulas.free_var fm in
  if (free_var = []) then
    (if (Formulas.holds m v fm)
     then Format.printf "true\n"
     else Format.printf "false\n")
  else
    let model = Formulas.denotations m fm in
    if (model = []) then Format.printf "false\n"
    else
      List.iteri
        (fun i m ->
          Format.printf "valuation nb%i:\n" i;
          (List.iter (fun v -> Format.printf "%s = " v;
                               Domain.print_domain (m(v))) free_var);
          Format.printf "\n")
        model

let () =
  let () =
    Arg.parse
      options
      (fun f -> files := f::(!files))
      (Sys.argv.(0) ^
       " stories\n outil") in
  let () = set_flags () in
  let () = parse_fm () in
  let posets = Domain.set_posets (!files) in
  let m = Formulas.interpretation posets in
  List.iteri
    (fun i fm ->
      Format.printf "evaluate formula %i:\n" i; (evaluate fm m empty_valuation))
    (!read_fm)

(*  let (valuation,fm) = test_subset posets in
  if (Formulas.holds m valuation fm) then Format.printf"true\n"
  else Format.printf "false\n"
 *)
