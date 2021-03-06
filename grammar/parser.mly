%token <string> VAR
%token <string> LABEL
%token <string> CONST
%token LPAREN RPAREN CONFIG
%token FALSE TRUE AND OR NOT IMP IFF FORALL EXISTS
%token P_IN P_SAME_LABELS P_EQ_LAB P_EQ_POS P_SUBPOS P_EQ_EV PREC_1 PREC_STAR
%token F_INTRO P_NEG_INFL
%token EOL
%token SORT_E SORT_P
%start main     /* the entry point */
%type <string Formulas.fol IntermediateFormula.interFormula> main
%%

main:
	| expr EOL		     { IntermediateFormula.Formula $1 }
	| variable_def EOL	     { IntermediateFormula.Var $1 }
        | EOL main 		     { $2 }
;

expr:
	| TRUE				     { Formulas.True }
	| FALSE				     { Formulas.False }
	| LPAREN NOT expr RPAREN	     { Formulas.Not $3 }
	| LPAREN expr AND expr RPAREN	     { Formulas.And ($2, $4) }
	| LPAREN expr OR expr RPAREN	     { Formulas.Or ($2, $4) }
	| LPAREN expr IMP expr RPAREN	     { Formulas.Imp ($2, $4) }
	| LPAREN expr IFF expr RPAREN	     { Formulas.Iff ($2, $4) }
	| FORALL VAR sort expr 		     { Formulas.Forall ($2, $3, $4) }
	| EXISTS VAR sort expr 		     { Formulas.Exists ($2, $3, $4) }
	| LPAREN term pred term RPAREN       { Formulas.Atom(Formulas.R($3, [ $2;$4 ])) }
	| LPAREN term pred term term RPAREN  { Formulas.Atom(Formulas.R($3, [ $2;$4;$5 ])) }
	| LPAREN term pred RPAREN  	     { Formulas.Atom(Formulas.R($3, [ $2 ])) }
	| LPAREN term term pred term term RPAREN { Formulas.Atom(Formulas.R($4, [ $2;$3;$5;$6 ])) }

;
term:
	 VAR 	      	  		{ Formulas.Var($1) }
	 | CONST			{ Formulas.Const($1) }
	 | F_INTRO LPAREN term RPAREN	{ Formulas.Fn("intro", [$3]) }
;
pred:
	| P_SUBPOS			{ "sub_posets" }
	| P_IN				{ "in" }
	| P_SAME_LABELS			{ "equal_event_labels" }
 	| P_NEG_INFL			{ "negative_influence" }
	| P_EQ_LAB LABEL		{ String.concat "" ["label"; $2] }
	| P_EQ_POS 			{ "equal_posets" }
	| P_EQ_EV 			{ "equal_events" }
	| PREC_1			{ "prec_1" }
	| PREC_STAR			{ "prec_star" }
;
sort:
	SORT_E				{ "Event" }
	| SORT_P			{ "Poset" }
variable_def:
	| CONFIG VAR P_EQ_EV CONST	{ ($2,$4) }
;