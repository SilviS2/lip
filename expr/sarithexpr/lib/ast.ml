type expr =
    True
  | False
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr
  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr

let rec is_nv : expr -> bool = function
  | Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false
