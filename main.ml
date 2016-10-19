let files = ref []
let formula_file = ref ""
let read_fm = ref []
let debug_mode = ref false

let options = [
    ("-f", Arg.Set_string formula_file, "file name skeleton for outputs");
    ("-debug", Arg.Set debug_mode, "print internal info");]

let test_z3 t =
  let posets = Domain.get_posets t in
  let p1 = List.nth posets 0 in
  let p2 = List.nth posets 3 in
  Z3morphism.get_morphism p1 p2

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

let evaluate sfm (_,_, domain as m) v =
  let fm = Formulas.convert_string_to_domain sfm domain in
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
(*  test_z3 posets*)
  let m = Formulas.interpretation posets in
 List.iteri
    (fun i fm ->
      Format.printf "\n evaluate formula %i:\n" i;
      (evaluate fm m empty_valuation))
    (!read_fm)
(*  let (valuation,fm) = test_subset posets in
  if (Formulas.holds m valuation fm) then Format.printf"true\n"
  else Format.printf "false\n"
 *)
