open Ast

type value =
  | VClosure of { param : var; body : expr; env : env  }
  | VInt of int
  | VBool of bool

and env = (var * value) list

let rec eval (env : env) (expr : expr) : value =
  match expr with
  | Variable { name } -> List.assoc name env
  | Abstraction { param; body } -> VClosure { param; body; env }
  | Application { abstraction; argument } -> eval_app env abstraction argument
  | Integer v -> VInt v
  | Boolean b -> VBool b
  | BinOp (bop, e1, e2) -> eval_bop env bop e1 e2

and eval_app env abstraction argument =
  let arg = eval env argument in
  let abs = eval env abstraction in
  match abs with
  | VClosure { param; body; env } ->
      let new_env = (param, arg) :: env in
      eval new_env body
  | _ -> failwith "Invalid function application "

and eval_bop (env : env) (bop : binop) (e1 : expr) (e2 : expr) : value =
  let v1 = get_integer (eval env e1) in
  let v2 = get_integer (eval env e2) in
  match bop with
  | Plus -> VInt (v1 + v2)

and get_integer = function
  | VInt v -> v
  | _ -> failwith "Expected integer at binary operation"

let value_to_string = function
  | VClosure _ -> "VClosure - to_string not implemented"
  | VInt v -> string_of_int v
  | VBool b -> string_of_bool b
