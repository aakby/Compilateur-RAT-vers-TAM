type typ = Bool | Int | Rat | Pointeur of typ | Enumere of string | Undefined 

let rec string_of_type t = 
  match t with
  | Bool ->  "Bool"
  | Int  ->  "Int"
  | Rat  ->  "Rat"
  | Pointeur typ -> "* "^(string_of_type typ)
  | Undefined -> "Undefined"
  | Enumere n -> "enum "^n

let rec est_compatible t1 t2 =
  match t1, t2 with
  | Bool, Bool -> true
  | Int, Int -> true
  | Rat, Rat -> true 
  | Pointeur t1, Pointeur t2 -> est_compatible t1 t2
  | Pointeur _, Undefined -> true 
  | Enumere n1, Enumere n2 -> n1 = n2
  | _ -> false 

let est_compatible_list lt1 lt2 =
  try
    List.for_all2 est_compatible lt1 lt2
  with Invalid_argument _ -> false

let getTaille t =
  match t with
  | Int -> 1
  | Bool -> 1
  | Rat -> 2
  | Pointeur _ -> 1
  | Undefined -> 0
  | Enumere _ -> 1
  
