(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let addlist l = List.fold_left(fun acc x->x+acc) 0 l;