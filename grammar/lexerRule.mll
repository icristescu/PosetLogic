{
open ParserRule(* The type token is defined in parser.mli *)
open Lexing
exception Eof

let reach_eof lexbuf =
    lexbuf.lex_eof_reached <- true
}

let eol = '\r'? '\n'
let blank = [' ' '\t']
let integer = '-'? (['0'-'9']+)
let real =
  '-'?
     ((((['0'-'9']+ ('.' ['0'-'9']*)?) | ('.' ['0'-'9']+))
	['e' 'E'] ['+' '-']? ['0'-'9']+)
  | ((['0'-'9']+ '.' ['0'-'9']* ) | (['0'-'9']* '.' ['0'-'9']+)))
let id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '+']* )
let internal_state = '~' (['0'-'9' 'a'-'z' 'A'-'Z' '_' '-' '+']+)


rule token = parse
	 | '\\' blank* eol {Lexing.new_line lexbuf ; token lexbuf}
	 | "<->" {KAPPA_LRAR}
	 | "->" {KAPPA_RAR}
	 | "<-" {LAR}
	 | eol {Lexing.new_line lexbuf ; NEWLINE}
	 | integer as n {try INT (int_of_string n)
	   	         with Failure _ -> raise (ExceptionDefn.Syntax_Error
	 		 (n^" is a incorrect integer"))}
	 | real as f {FLOAT (float_of_string f)}
	 | '\'' ([^'\n''\'']+ as x) '\''{LABEL(x)}
	 | id as str {ID str}
	 | '@' {AT}
	 | ',' {COMMA}
	 | '(' {OP_PAR}
	 | ')' {CL_PAR}
	 | '.' {DOT}
	 | '|' {PIPE}
	 | '%' (id as lab) ':' {
		match lab with
		| "init" -> INIT
		| "obs" -> OBS
		| _ ->
		    raise (ExceptionDefn.Syntax_Error("invalid use of %"))}
	 | '!' {KAPPA_LNK}
	 | internal_state as s {let i = String.index s '~' in
				let r = String.sub s (i+1) (String.length s-i-1) in
				KAPPA_MRK r
			       }
	 | '?' {KAPPA_WLD}
	 | '_' {KAPPA_SEMI}
	 | blank  {token lexbuf}
	 | eof {raise Eof}
  	 | _ {raise (ExceptionDefn.Syntax_Error("invalid use of character "))}