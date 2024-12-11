%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token <string> VAR
%token <string> CONST
%token NOT
%token AND
%token OR
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token EOF

%token SKIP
%token ASSIGN
%token SEQ
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO

%left SEQ
%right ELSE
%right ADD SUB MUL
%right AND OR
%right LEQ
%right WHILE
%right NOT
%right DO

%start <cmd> prog

%%

prog:
  | e = cmd; EOF { e }
;

expr:
   TRUE { True }
  | FALSE { False }
  | n = VAR { Var(n) }
  | n = CONST { Const(int_of_string n) }
  | NOT; e1 = expr; { Not(e1) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1, e2) }
  | e1 = expr; ADD; e2 = expr { Add(e1,e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1,e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1,e2) }
  | e1 = expr; EQ; e2 = expr { Eq(e1,e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;

cmd:
   SKIP { Skip }
  | v = VAR ; ASSIGN; e2 = expr; { Assign(v, e2) }
  | c1 = cmd; SEQ; c2 = cmd; { Seq(c1, c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e, c1, c2) }
  | IF; e = expr; THEN; LPAREN; c1 = cmd; RPAREN; ELSE; c2 = cmd; { If(e, c1, c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; LPAREN; c2 = cmd; RPAREN; { If(e, c1, c2) }
  | WHILE; e = expr; DO; c = cmd; { While(e, c) }
  | WHILE; e = expr; DO; LPAREN; c = cmd; RPAREN; { While(e, c) }
;

