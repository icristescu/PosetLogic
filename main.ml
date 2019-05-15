let files = ref []
let env_file = ref ""
let formula_file = ref ""
let read_fm = ref []
let debug_mode = ref false
let verb = ref false

let options = [
    ("-f", Arg.Set_string formula_file, "file name for formulas");
    ("-env", Arg.Set_string env_file, "file name for environment");
    ("-debug", Arg.Set debug_mode, "print internal info");
    ("-verb", Arg.Set verb, "print every valuation");]

let empty_valuation = function
    _ -> failwith "empty valuation"

let parse_fm () =
  let () =
    if (!Param.debug_mode) then
      Format.printf "@.********* the formulas in file %s are:\n"
                    (!formula_file) in
  let chan = open_in (!formula_file) in
  try
    let lexbuf = Lexing.from_channel chan in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      read_fm := result::(!read_fm);
      if (!Param.debug_mode) then
        (IntermediateFormula.print result;Format.printf"@.")
    done
  with Lexer.Eof -> ()

let set_flags () =
  Param.debug_mode := !debug_mode;
  Param.verb := !verb

(** A formula 'sfm' is evaluated to true or false given a model 'm' and a
    valuation of (some of) its free variable. If the valuation is not defined
    for all free variables, possible valuations are suggested by the model. *)
let evaluate sfm (_,_,domain as m) v =
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
      (Sys.argv.(0) ^" stories\n outil") in
  let () = set_flags () in
  let json = Yojson.Basic.from_file (!env_file) in
  let env = Model.of_yojson json in
  let posets = Domain.set_posets (!files) (Some env) in
  let m = Formulas.interpretation env posets in
  let () = parse_fm () in
  let formulas = IntermediateFormula.prepare_formulas (!read_fm) in
  List.iteri
    (fun i fm ->
      Format.printf "@.********* evaluate formula %i:\n" i;
      (evaluate fm m empty_valuation))
    formulas
