let rule_file = ref ""
let read_rule = ref []
let options = [
    ("-f", Arg.Set_string formula_file, "file name for formulas");
    ("-r", Arg.Set_string rule_file, "file name for rules");
    ("-debug", Arg.Set debug_mode, "print internal info");]


let parse_rules () =
  let () = if (!Param.debug_mode) then
             Format.printf "parse file %s\n" (!rule_file) in
  let chan = open_in (!rule_file) in
  try
    let lexbuf = Lexing.from_channel chan in
    while true do
      let result = ParserRule.newline LexerRule.token lexbuf in
      let rule = Ast.clean_rule result in
      read_rule := rule::(!read_rule);
      if (!Param.debug_mode) then
        (Format.printf "parsing \n"; Ast.print result;
         Format.printf " becomes \n"; Rule.print rule;Format.printf "\n")
    done
  with LexerRule.Eof -> ()


(*
let nodes_of_json (node:Yojson.Basic.json) =
  let open Yojson.Basic.Util in
  match node with
  | `List [`Int id; `String "RULE"; `String label;
           (`Assoc ["quarks", `List l]) ]
    | `List [`Int id; `String "PERT"; `String label;
             (`Assoc ["quarks", `List l])]->
     let quarks_ls =
       List.map (fun q -> Quark.quarks_of_json q) l in
     let clean_quarks =
       List.filter (fun q -> Quark.positive_site q) quarks_ls in
     { event_id = id; event_label = label; quarks = clean_quarks; }
  | `List [`Int id; `String "OBS"; `String label;
           (`Assoc ["quarks", `List l])] ->
     let quarks_ls =
       List.map (fun q -> Quark.quarks_of_json q) l in
     let clean_quarks =
       List.filter (fun q -> Quark.positive_site q) quarks_ls in
     let () = if ((Quark.exists_mod clean_quarks [0;1])
                  &&(Quark.exists_testmod clean_quarks [0;1])) then
                (raise (ExceptDefn.NotKappa_Poset
                          ("quarks of obs event not valid"))) in
     { event_id = id; event_label = label; quarks = clean_quarks; }
  | `List [`Int id; `String "INIT"; `List l;
           (`Assoc ["quarks", `List ql])] ->
     let init_label =
       List.fold_left
         (fun lbl n ->
           match n with
             | `String i -> lbl^i
             | x -> raise (Yojson.Basic.Util.Type_error
                             ("Not in the cflow format",x))) "" l in
     let quarks_ls =
       List.map (fun q -> Quark.quarks_of_json q) ql in
     let clean_quarks =
       List.filter (fun q -> Quark.positive_site q) quarks_ls in
     let () = if ((Quark.exists_test clean_quarks [0;1])
                  &&(Quark.exists_testmod clean_quarks [0;1])) then
                (raise (ExceptDefn.NotKappa_Poset
                          ("quarks of init event not valid"))) in
     { event_id = id; event_label = init_label; quarks = clean_quarks; }
  | _ -> raise (Yojson.Basic.Util.Type_error ("Not in the cflow format",`Null))
 *)
