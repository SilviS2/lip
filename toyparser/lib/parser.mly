%{
open Ast
%}

%token <string> CONST
%token PLUS
%token MINUS
%token MOL
%token DIV
%token LPAREN
%token RPAREN
%token EOF

%left MOL
%left DIV
%left MINUS
%left PLUS

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Dif(e1,e2) }
  | e1 = expr; MOL; e2 = expr { Molt(e1,e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1,e2) }
  | MINUS; e1 = expr { Meno(e1) }
  | LPAREN; e=expr; RPAREN {e}
;
