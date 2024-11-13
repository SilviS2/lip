open Toylexer.Main
open Toylexer.Token

let%test "test_frequencies_1" =
  lexer "x=y; x=x+1" |> frequency 3 = [(ID "x", 3); (ASSIGN, 2); (ID "y", 1)]

(* YOUR TESTS HERE *)
let%test "test_frequencies_2" =
  lexer "0X2" |> frequency 3 = [(ETOK "0X2", 1); (EOF, 1)]
