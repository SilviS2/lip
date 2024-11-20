open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | And(e0, e1) -> "And(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ ")"
  | Or(e0, e1) -> "Or(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ ")"
  | Not(e0) -> "Not(" ^ (string_of_boolexpr e0) ^ ")"


let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e1,e2,e3) when (e1=True || e1=False) && (e2<>True || e2<>False) -> 
    let result = trace1 e2 in 
    If(e1, result, e3)
  | If(e1,e2,e3) when e1=True || e1=False -> 
    let result = trace1 e3 in 
    If(e1, e2, result)
  | If(e1,e2,e3) -> 
    let result = trace1 e1 in 
    If(result, e2, e3)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval = function
    True -> true
  | False -> false
  | If(e1,e2,_) when eval e1 -> eval e2
  | If(_,_,e2) -> eval e2
  | And(e1, e2) when eval e1 -> eval e2
  | And(_, _) -> false
  | Or(e1, _) when eval e1 -> true
  | Or(_, e2) -> eval e2
  | Not(e1) when (eval e1) -> false
  | Not(_) -> true