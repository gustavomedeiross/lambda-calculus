let f = fun x -> fun y -> plus x y in
let double = fun x : int -> plus x x in
let x = f 10 2 in
double x
