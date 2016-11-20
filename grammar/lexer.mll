{
open Parser(* The type token is defined in parser.mli *)
exception Eof
exception LexingError
}
let id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '+']* )
let blank = [' ' '\t']


rule token = parse
    [' ' '\t']		{ token lexbuf }     (* skip blanks *)
  | ['\n' ]		{ EOL }
  | '('        	  	{ LPAREN }
  | ')'    		{ RPAREN }
  | "true" 		{ TRUE }
  | "false" 		{ FALSE }
  | "not"		{ NOT }
  | "and"		{ AND }
  | "or"		{ OR }
  | "iff"		{ IFF }
  | "imp"		{ IMP }
  | "forall"		{ FORALL }
  | "exists"		{ EXISTS }
  | '<'			{ PREC_1 }
  | "=<"		{ PREC_STAR }
  | "in"		{ P_IN }
  | "same_labels"	{ P_SAME_LABELS }
  | "equal_label"	{ P_EQ_LAB }
  | "equal_posets"	{ P_EQ_POS }
  | "subset"		{ P_SUBPOS }
  | "intro"		{ F_INTRO }
  | "-|"		{ P_NEG_INFL }
  | ":Event"		{ SORT_E }
  | ":Poset"		{ SORT_P }
  | '\"'		{ let str = read_label [] ['\"'] lexbuf in LABEL str}
  | '\''		{ let str = read_label [] ['\''] lexbuf in CONST str}
  | '='			{ P_EQ_EV }
  | id as i		{ VAR i }
  | eof    		{ raise Eof }
  | _ 			{ raise LexingError }
and read_label acc char_list =
  parse
  | eof			{ String.concat "" (List.rev_map (fun x -> String.make 1 x) acc) }
  | _ as c 		{ if List.mem c char_list
	    		  then String.concat "" (List.rev_map (fun x -> String.make 1 x) acc)
	    		  else read_label (c::acc) char_list lexbuf}
