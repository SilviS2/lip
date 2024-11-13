{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let esadecimali = ['0']['x' 'X'] (['a'-'f' 'A'-'F'] | num num* ['a'-'f' 'A'-'F']*)+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MOL }
  | "/" { DIV }
  | esadecimali { CONST (Lexing.lexeme lexbuf)}
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
