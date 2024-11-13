{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let atok = ['A' - 'Z'] chr*
let btok = ['a' 'e' 'i' 'o' 'u']+
let cons_min = ['b'-'d' 'f'-'h' 'j'-'n' 'p'-'t' 'v'-'z']
let cons_maiusc = ['B'-'D' 'F'-'H' 'J'-'N' 'P'-'T' 'V'-'Z']
let voc_min = ['a' 'e' 'i' 'o' 'u']
let ctok = cons_min* cons_maiusc* voc_min* cons_min* cons_maiusc*
let dtok = ['-']* num* ['.']* num+
let etok = ['0']['x' 'X'] (['a'-'f' 'A'-'F'] | num num* ['a'-'f' 'A'-'F']*)+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | atok { ATOK (Lexing.lexeme lexbuf) } 
  | btok { BTOK (Lexing.lexeme lexbuf) } 
  | ctok { CTOK (Lexing.lexeme lexbuf) } 
  | etok { ETOK (Lexing.lexeme lexbuf) } 
  | dtok { DTOK (Lexing.lexeme lexbuf) } 
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }   
  | eof { EOF }
