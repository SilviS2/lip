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
(*conta il numero di token uguali a t presenti nella lista l*)
let conta t l = List.length (List.filter (fun x ->x=t) l)

(*restituisce i primi n elementi della lista l*)
let rec tronco n l = match l with
    []->[]
  | _::rest when n>0 -> if n>0 then tronco (n-1) rest else rest
  | _ -> l
;; 

(*data una lista l, se un elemento è già stato "visto" 
allora non viene aggiunto alla lista finale, altrimenti sì*)
let remove_duplicates l =
  let rec aux seen result = function
    | [] -> result
    | x::rest ->
        if List.mem x seen then  
          aux seen result rest
        else
          aux (x::seen) (x::result) rest 
  in
  aux [] [] l;; 


let frequency n l = 
  (*crea una lista con la coppia token e numero di volte in cui è presente nella lista*)
  let conti = List.map (fun x -> (x, conta x l)) l in 
  (*crea una lista senza duplicati*)
  let duplicati = remove_duplicates conti in
  (*crea una lista con solo i primi n elementi*)
  let tronchi = tronco ((List.length duplicati) - n) duplicati in
  (*ordina la lista in base al numero di volte in cui sono presnti i token*)
  List.rev(List.sort (fun (_, count1) (_, count2) -> compare count1 count2) tronchi)
;; 
