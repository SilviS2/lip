open Boolexpr.Main
open Boolexpr.Ast

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

(* ### Unit tests for task 4 *)

let%test "test_eval_1" = test_eval "false" false
let%test "test_eval_2" = test_eval "true" true
let%test "test_eval_3" = test_eval "if true then false else true" false
let%test "test_eval_4" = test_eval "if false then false else true" true
let%test "test_eval_5" = test_eval "if true then (if true then false else true) else (if true then true else false)" false

let%test "test_eval_6" = test_eval "if (if false then false else false) then (if false then true else false) else (if true then false else true)" false

let%test "test_eval_7" = test_eval "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" false


(* ### Unit tests for task 5 *)

let%test "test_trace1_progress_1" =
  match trace1 (If (True, False, True)) with
  | exception NoRuleApplies -> false
  | _ -> true

let%test "test_trace1_progress_2" =
  match is_value (If (True, False, True)) with
  | exception NoRuleApplies -> false
  | _ -> true

let%test "test_trace1_progress_3" =
  let expr = If (If (False, False, False), If (False, True, False), If (True, False, True)) in
  let rec reduce expr steps =
    if steps > 10 then false
    else if is_value expr then true
    else reduce (trace1 expr) (steps + 1)
  in
  reduce expr 0