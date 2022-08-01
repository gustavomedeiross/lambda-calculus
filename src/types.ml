type typ =
  | TInt
  | TBool
  | TArrow of typ * typ

let rec type_to_string = function
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (t1, t2) ->
    let t1 = type_to_string t1 in
    let t2 = type_to_string t2 in
    t1 ^ " -> " ^ t2

let rec equals t1 t2 =
  match (t1, t2) with
  | (t1, t2) when t1 == t2 -> true
  | (TArrow (p1, r1), TArrow (p2, r2)) ->
    equals p1 p2 && equals r1 r2
  | _ -> false
