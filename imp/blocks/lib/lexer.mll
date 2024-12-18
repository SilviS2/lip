{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z']*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "int" { INTVAR }
  | "bool" { BOOLVAR }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "=" { EQ }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "<=" { LEQ }
  | "skip" { SKIP }
  | ":=" { ASSIGN }
  | ";" { SEQ }
  | "if" { IF }
  | "else" { ELSE }
  | "then" { THEN }
  | "while" { WHILE }
  | "do" { DO }
  | letter { VAR (Lexing.lexeme lexbuf) } 
  | num { CONST ( Lexing.lexeme lexbuf) }
  | eof { EOF }
