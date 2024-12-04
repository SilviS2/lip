open Ast

type exprtype = BoolT | NatT
type exprval = Bool of bool | Nat of int

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Zero -> "zero"
  | Not(e0) -> "Not(" ^ (string_of_expr e0) ^ ")"
  | And(e0, e1) -> "And(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | Or(e0, e1) -> "Or(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ ")"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | IsZero(e0) -> "iszero" ^ (string_of_expr e0)
  | Succ(e0) -> "succ" ^ (string_of_expr e0)
  | Pred(e0) -> "pred" ^ (string_of_expr e0) 


let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
exception NoRuleApplies

let rec trace1 = function
  | If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e1, e2, e3) -> If(trace1 e1, e2, e3)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e1) -> Not(trace1 e1)
  | And(True, e2) -> e2
  | And(False, _) -> False
  | And(e1, e2) -> And(trace1 e1, e2)
  | Or(True, _) -> True
  | Or(False, e2) -> e2
  | Or(e1, e2) -> Or(trace1 e1, e2)
  | Succ(e1) -> Succ(trace1 e1)
  | Pred(Succ(e)) when is_nv e -> e
  | Pred(e) -> Pred(trace1 e)
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when is_nv e -> False
  | IsZero(e) -> IsZero(trace1 e)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
  
let rec int_of_nat = function
    Zero -> 0
  | Succ n -> 1 + int_of_nat n
  | _ -> failwith "int_of_nat on non-nat"

let string_of_val = function
  Bool b -> string_of_bool b
  | Nat n -> string_of_int n

exception TypeError of string

let rec typecheck = function
   True -> BoolT
  | False -> BoolT
  | Not a -> (match typecheck a with 
    BoolT -> BoolT
    | _ -> raise(TypeError "Errore"))
  | And (a, b) -> (match typecheck a, typecheck b with 
    (BoolT, BoolT) -> BoolT
  | _ -> raise(TypeError "Errore"))
  | Or (a, b) -> (match typecheck a, typecheck b with 
    (BoolT, BoolT) -> BoolT
  | _ -> raise(TypeError "Errore"))
  | If (a, b, c) -> (match typecheck a, typecheck b, typecheck c with 
    (BoolT, BoolT, BoolT) -> BoolT
    | _ -> raise(TypeError "Errore"))
  | Zero -> NatT
  | Succ a -> (match typecheck a with 
    NatT -> NatT
    | _ -> raise(TypeError "Errore"))
  | Pred a -> (match typecheck a with 
    NatT -> NatT
    | _ -> raise(TypeError "Errore"))
  | IsZero a -> (match typecheck a with 
    NatT ->   BoolT
    | _ -> raise(TypeError "Errore"))
;;

let string_of_type a = match a with
   BoolT -> "BoolT"
  | NatT -> "NatT"

let rec eval = function
   True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | Not(e1) when (eval e1) = Bool true -> Bool false
  | Not(e1) when (eval e1) = Bool false -> Bool true
  | Not(_) -> failwith "Errore, input non valido"
  | And(e1, e2) -> (
      match eval e1 with
      | Bool true -> eval e2
      | Bool false -> Bool false
      | _ -> failwith "And expects booleans"
    )
  | Or(e1, _) when eval e1 = Bool true -> Bool true
  | Or(_, e2) -> eval e2
  | If(e1,e2,_) when eval e1 = Bool true -> eval e2
  | If(_,_,e2) -> eval e2
  | IsZero(e1) -> (
    match eval e1 with
    | Nat 0 -> Bool true
    | Nat _ -> Bool false
    | _ -> failwith "Errore, l'input non è un numero"
  )
  | Succ(e1) -> (
    match eval e1 with
    | Nat n -> Nat (n + 1)
    | _ -> failwith "Succ expects a natural number"
  )
  | Pred(e1) -> (
      match eval e1 with
      | Nat 0 -> failwith "Errore, non esiste un precedente di 0"
      | Nat n -> Nat (n - 1)
      | _ -> failwith "Pred expects a natural number"
  )