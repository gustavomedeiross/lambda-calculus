module TyVar : sig
  type t [@@deriving show]

  type state

  val gen_fresh : state -> (t * state)

  val initial_state : unit -> state

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val to_string : t -> string
end
=
struct
  type t = string [@@deriving show]

  type state = VarState of int

  let of_int v = "?X" ^ string_of_int v

  let from_int v = 
    v
    |> Str.global_replace (Str.regexp "?X") ""
    |> int_of_string

  let initial_state () = VarState 0

  let gen_fresh (VarState s) =
    let fresh = s + 1 in
    (of_int fresh, VarState fresh)

  let compare x y =
    Int.compare (from_int x) (from_int y)

  let equal = String.equal

  let to_string s = s
end

type var = TyVar.t
[@@deriving show]

type typ =
  | TInt
  | TBool
  | TArrow of typ * typ
  | TVar of var
[@@deriving show]

(* forall 'a 'b . 'a -> 'b *)
type scheme = Scheme of (var list * typ)
[@@deriving show]

let rec type_to_string = function
  | TInt -> "int"
  | TBool -> "bool"
  | TVar var -> TyVar.to_string var
  | TArrow (TArrow _ as t1, t2) ->
    let t1 = "(" ^ type_to_string t1 ^ ")" in
    let t2 = type_to_string t2 in
    t1 ^ " -> " ^ t2
  | TArrow (t1, t2) ->
    let t1 = type_to_string t1 in
    let t2 = type_to_string t2 in
    t1 ^ " -> " ^ t2

let rec equal t1 t2 =
  match (t1, t2) with
  | (t1, t2) when t1 == t2 -> true
  | (TArrow (p1, r1), TArrow (p2, r2)) ->
    equal p1 p2 && equal r1 r2
  | _ -> false
