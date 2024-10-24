open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let rec conta n l = match l with
    []-> 0
  | x::rest when x=n-> 1+conta n rest
  | _::rest -> conta n rest;; 

let rec tronco n l = match l with
    []->[]
  | _::rest when n>0 -> if n>0 then tronco (n-1) rest else rest
  | _ -> l
;; 

let frequency n l = 
  let unique_elements = List.sort_uniq compare l in 
  let tronchi_elements = tronco ((List.length unique_elements) - n) unique_elements in 
  let final = List.map (fun x -> (x, conta x l)) tronchi_elements in
  List.rev(List.sort (fun (_, count1) (_, count2) -> compare count1 count2) final)
;;
