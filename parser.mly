%{
open Region
open RegionInferenceAlgorithm
open TypedExp
%}
%token <string> IDENT
%token LET DEF IN LPAREN RPAREN LAMBDA DOT COLON ARROW INT EOF
%token EOF
%type <TypedExp.t> parse
%start parse
%%

parse :| expr { $1 }

atty  :| INT  { MLType.Int }
       | LPAREN ty RPAREN { $2 }

ty    :| atty { $1 }
       | atty ARROW ty { MLType.Arrow($1, $3) }

atom  :| IDENT { Var ($1, []) }
       | LPAREN expr RPAREN { $2 }

app   :| atom { $1 }
       | app atom { App($1, $2) }

expr  :| app { $1 }
       | LAMBDA IDENT COLON ty DOT expr { Abs($2, $4, $6) }
       | LET IDENT COLON ty DEF expr IN expr { App(Abs($2, $4, $8), $6) }
