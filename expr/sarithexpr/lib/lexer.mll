{
open Parser
}

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "0" { ZERO }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "iszero" { ISZERO }
  | "succ" { SUCC }
  | "pred" { PRED }
  | eof { EOF }
