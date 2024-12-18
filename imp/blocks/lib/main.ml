open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
ast

let rec eval_expr (st:state) = function  
 True -> Bool true
| False -> Bool false
| Var e1 -> (
  match topenv st e1 with
  | BVar loc | IVar loc -> getmem st loc
)
| Const(e1) -> Int e1
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
  | Int n1, Int n2 -> Int (n1 + n2)  (* Add two natural numbers *)
  | _ -> failwith "Add expects natural numbers"
)
| Sub (e1,e2) -> (
  match eval_expr st e1, eval_expr st e2 with
  | Int n1, Int n2 -> Int (n1 - n2)  (* Add two natural numbers *)
  | _ -> failwith "Sub expects natural numbers"
)
| Mul (e1,e2) -> (
  match eval_expr st e1, eval_expr st e2 with
  | Int n1, Int n2 -> Int (n1 * n2)  (* Add two natural numbers *)
  | _ -> failwith "Add expects natural numbers"
)
| Eq (e1,e2) -> (
  match eval_expr st e1, eval_expr st e2 with
  | Int n1, Int n2 -> Bool (n1 = n2)  (* Add two natural numbers *)
  | _ -> failwith "Eq expects natural numbers"
)
| Leq (e1,e2) -> (
  match eval_expr st e1, eval_expr st e2 with
  | Int n1, Int n2 -> Bool (n1 <= n2)  (* Add two natural numbers *)
  | _ -> failwith "Leq expects natural numbers"
)

let eval_decl : state -> decl list -> state =
  fun st ds ->
   let env, loc =
     List.fold_left
       (fun (env, loc) d ->
         match d with
         | IntVar x -> (bind_env env x (IVar loc), loc + 1)
         | BoolVar x -> (bind_env env x (BVar loc), loc + 1))
       (topenv st, getloc st)
       ds
   in
   let envstack = getenv st in
   make_state (env :: envstack) (getmem st) loc
   
let rec trace1 = function
    Cmd(Skip, st) -> St st
    | Cmd(Assign(x, e), st) -> 
      let env, mem = (topenv st, getmem st) in
      let new_mem =
        match eval_expr st e with
        | Int n -> (
            match env x with
            | IVar i -> bind_mem mem i (Int n)
            | _ -> failwith "not compatible")
        | Bool b -> (
            match env x with
            | BVar i -> bind_mem mem i (Bool b)
            | _ -> failwith "not compatible")
      in
      St (make_state (getenv st) new_mem (getloc st))
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
    | Cmd (Decl (dl, c), st) -> (
      let new_st = eval_decl st dl in
      match trace1 (Cmd (c, new_st)) with
      | St st' -> St (make_state (popenv st') (getmem st') (getloc st'))
      | Cmd (c', st') -> Cmd (Block c', st'))
  | Cmd (Block c, st) -> (
      match trace1 (Cmd (c, st)) with
      | St st' -> St (make_state (popenv st') (getmem st') (getloc st'))
      | Cmd (c', st') -> Cmd (Block c', st'))
    | _ -> raise NoRuleApplies

let trace n c =
  let initial_conf = Cmd(c, state0) in
  let rec trace_aux n conf =
    if n <= 0 then [conf]
    else 
      try 
        let conf' = trace1 conf in
        conf :: trace_aux (n-1) conf'
    with NoRuleApplies -> [conf]
  in trace_aux n initial_conf