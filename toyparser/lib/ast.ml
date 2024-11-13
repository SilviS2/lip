type ast =
    Const of int
  | Add of ast * ast
  | Dif of ast * ast
  | Molt of ast * ast
  | Div of ast * ast
  | Meno of ast
