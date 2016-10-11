{
open Parser(* The type token is defined in parser.mli *)
exception Eof
}
let id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '+']* )
let label = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '+']* )
let integer = '-'? (['0'-'9']+)

rule token = parse
    [' ' '\t']		{ token lexbuf }     (* skip blanks *)
  | ['\n' ]		{ EOL }
  | integer as lxm 	{ INT(int_of_string lxm) }
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
  | "in"		{ P_IN }
  | "same_labels"	{ P_SAME_LABELS }
  | "equal_label"	{ P_EQ_LAB }
  | "equal_posets"	{ P_EQ_POS }
  | "subset"		{ P_SUBPOS }
  | "intro"		{ F_INTRO }
  | id as i		{ VAR i }
  | label as l		{ LABEL l }
  | eof    		{ raise Eof }
