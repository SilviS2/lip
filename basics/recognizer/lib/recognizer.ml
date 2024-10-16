let rec lang1 l = match l with 
  | ['0'] -> true
  | ['1'] -> true
  | _::rest -> lang1 rest
  | _ -> false
;; 

let rec allone l = match l with
    [] -> true
  | x:: rest when x = '1' -> allone rest
  | _ -> false
;;

let lang2 l = match l with 
    [] -> true
  | ['0'] -> true
  | x::rest when x = '0' -> allone rest
  | x::rest when x = '1' -> allone rest
  | _ -> false
;;

let rec aux l = match l with 
  | ['0';'1'] -> true
  | x::y::rest when List.length l mod 2 = 0 && x='0' && y='1' -> aux rest
  | _ -> false
;;

let lastzero l = match l with 
    [] -> false
  | x::rest when x = '0' -> aux (List.rev rest)
  | _ -> false
;;

let lang3 l = match l with 
    ['0'; '0'] -> true
  | x::rest when x = '0'  && lastzero (List.rev rest) -> true
  | _ -> false
;;

let rec counter l = match l with
    [] -> 0
  | x::rest when x = '1' -> 1+counter rest
  | _::rest -> counter rest                         
;;

let lang4 l = match l with 
    ['1'; '1'] -> true 
  | _ when counter l = 2 -> true
  | _ -> false 
;;

let rec lang5 l = match l with 
    ['0'; '0'] -> true
  | ['1'; '1'] -> true
  | x::y::rest when (x = '0' && y = '0') || (x = '1' && y = '1') -> lang5 rest 
  | _ -> false
;;

let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers;;
  
