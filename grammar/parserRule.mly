%token EOF NEWLINE SEMICOLON COMMA DOT OP_PAR CL_PAR OP_CUR CL_CUR AT TYPE LAR
%token GREATER SMALLER TRUE FALSE DIFF KAPPA_RAR KAPPA_LRAR KAPPA_LNK PIPE
%token INIT OBS KAPPA_WLD KAPPA_SEMI
%token <int> INT
%token <string> ID
%token <string> KAPPA_MRK LABEL
%token <float> FLOAT
%token <string> STRING
%start newline
%type <Ast.t> newline
%%

newline:
    | start_rule NEWLINE {$1}
;

start_rule:
    | rule_expression {$1}
    | instruction {$1}
    | error
	{raise (ExceptionDefn.Syntax_Error("start_rule"))}
    ;

rule_expression:
    | rule_label lhs_rhs arrow lhs_rhs
		 { Ast.RULE ($1,({Ast.lhs=$2; Ast.bidirectional=$3;
			    Ast.rhs=$4;}))}
    ;

arrow:
    | KAPPA_RAR {false}
    | KAPPA_LRAR {true}
    ;

rule_label:
    | LABEL {$1}
    ;

instruction:
    | INIT init_declaration {$2}
    | INIT error
	{ raise (ExceptionDefn.Syntax_Error("Malformed initial condition"))}
    | OBS variable_declaration {$2}
    ;

variable_declaration:
    | LABEL non_empty_mixture {Ast.OBS ($1,$2)}
;

init_declaration:
    | non_empty_mixture {(Ast.INIT $1)}
    ;

lhs_rhs:
  mixture {$1};

mixture:
      /*empty*/ {[]}
    | non_empty_mixture {$1}
;

non_empty_mixture:
    | OP_PAR non_empty_mixture CL_PAR {$2}
    | agent_expression COMMA non_empty_mixture {$1 :: $3}
    | agent_expression {[$1]}
    ;

agent_expression:
    | ID OP_PAR interface_expression CL_PAR {($1,$3)}
    | ID error
	 { raise (ExceptionDefn.Syntax_Error("Malformed agent '"^$1^"'"))}
    ;

interface_expression:
  /*empty*/ {[]}
    | ne_interface_expression {$1}
    ;

ne_interface_expression:
    | port_expression COMMA ne_interface_expression {$1::$3}
    | port_expression {[$1]}
    ;

port_expression:
    | ID internal_state link_state
	 {{Ast.port_nme=$1; Ast.port_int=$2; Ast.port_lnk=$3}}
    ;

internal_state:
     /*empty*/ {[]}
    | KAPPA_MRK internal_state {$1::$2}
    | error
	{raise (ExceptionDefn.Syntax_Error ("Invalid internal state"))}
    ;

link_state:
    /*empty*/ {[]}
    | KAPPA_LNK INT link_state {(Ast.LNK_VALUE $2)::$3}
    | KAPPA_LNK KAPPA_SEMI link_state {(Ast.LNK_SOME)::$3}
    | KAPPA_LNK ID DOT ID link_state {(Ast.LNK_TYPE($2,$4))::$5}
    | KAPPA_WLD link_state {(Ast.LNK_ANY)::$2}
    | KAPPA_LNK error
	{raise (ExceptionDefn.Syntax_Error("Invalid link state"))}
;
