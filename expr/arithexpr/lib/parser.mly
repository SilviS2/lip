%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token NOT
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token ZERO
%token ISZERO
%token SUCC
%token PRED
%token EOF

%right OR
%right AND
%right NOT
%right ISZERO
%right PRED SUCC

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
   TRUE { True }
  | FALSE { False }
  | ZERO { Zero }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | e1 = expr; OR; e2 = expr; { Or(e1, e2) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2) }
  | NOT; e1 = expr; { Not(e1) }
  | ISZERO; e1 = expr; { IsZero(e1) }
  | SUCC; e1 = expr; { Succ(e1) }
  | PRED; e1 = expr; { Pred(e1) }
  | LPAREN; e=expr; RPAREN {e}
;

