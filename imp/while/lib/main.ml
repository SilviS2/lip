open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
ast

let rec eval_expr (st:state) = function  
 True -> Bool true
| False -> Bool false
| Var e1 -> (try st e1 with _ -> raise (UnboundVar e1))
| Const(e1) -> Nat e1
| Not(e1) when (eval_expr st e1) = Bool true -> Bool false
| Not(e1) when (eval_expr st e1) = Bool false -> Bool true
| Not(_) -> failwith "Errore, input non valido"
| And(e1, e2) -> (
  match eval_expr st e1 with
  | Bool true -> eval_expr st e2
  | Bool false -> Bool false
  | _ -> failwith "And expects booleans"
)
| Or(e1, _) when eval_expr st e1 = Bool true -> Bool true
| Or(_, e2) -> eval_expr st e2
| Add (e1,e2) -> (
  match eval_expr st e1, eval_expr st e2 with
  | Nat n1, Nat n2 -> Nat (n1 + n2)  (* Add two natural numbers *)
  | _ -> failwith "Add expects natural numbers"
)
| Sub (e1,e2) -> (
  match eval_expr st e1, eval_expr st e2 with
  | Nat n1, Nat n2 -> Nat (n1 - n2)  (* Add two natural numbers *)
  | _ -> failwith "Sub expects natural numbers"
)
| Mul (e1,e2) -> (
  match eval_expr st e1, eval_expr st e2 with
  | Nat n1, Nat n2 -> Nat (n1 * n2)  (* Add two natural numbers *)
  | _ -> failwith "Add expects natural numbers"
)
| Eq (e1,e2) -> (
  match eval_expr st e1, eval_expr st e2 with
  | Nat n1, Nat n2 -> Bool (n1 = n2)  (* Add two natural numbers *)
  | _ -> failwith "Eq expects natural numbers"
)
| Leq (e1,e2) -> (
  match eval_expr st e1, eval_expr st e2 with
  | Nat n1, Nat n2 -> Bool (n1 <= n2)  (* Add two natural numbers *)
  | _ -> failwith "Leq expects natural numbers"
)

let bind st x v : state = fun y -> if x = y then v else st y

let rec trace1 = function
    Cmd(Skip, st) -> St st
    | Cmd(Assign(x, e), st) -> (match eval_expr st e with
      | Nat v -> St(bind st x (Nat v))
      | _ -> raise (TypeError "Expected integer")
      )
    | Cmd(Seq(c1, c2), st) -> (match trace1 (Cmd(c1, st)) with
      | St st' -> Cmd(c2, st')
      | Cmd(c1', st') -> Cmd(Seq(c1', c2), st')
      )
    | Cmd(If(e, c1, c2), st) -> (match eval_expr st e with
      | Bool false -> Cmd(c2, st)
      | Bool true -> Cmd(c1, st)
      | _ -> raise (TypeError "Expected boolean")
      )
    | Cmd(While(e, c), st) -> (match eval_expr st e with
      | Bool false -> St st
      | Bool true -> Cmd(Seq(c, While(e, c)), st)
      | _ -> raise (TypeError "Expected boolean")
    )
    | _ -> raise NoRuleApplies

let bottom : state = function _ -> Nat 0

let trace n c =
  let initial_conf = Cmd(c, bottom) in
  let rec trace_aux n conf =
    if n <= 0 then [conf]
    else 
      try 
        let conf' = trace1 conf in
        conf :: trace_aux (n-1) conf'
    with NoRuleApplies -> [conf]
  in trace_aux n initial_conf