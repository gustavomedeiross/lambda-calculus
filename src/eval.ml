open Ast

module Env = Map.Make(String)

type value =
  | VClosure of { param : var; body : expr; env : env  }
  | VNative of (value -> value)
  | VInt of int
  | VBool of bool

and env = value Env.t

let rec eval (env : env) (expr : expr) : value =
  match expr with
  | Variable { name } -> Env.find name env
  | Abstraction (param, _t, body) -> VClosure { param; body; env }
  | Application { abstraction; argument } -> eval_app env abstraction argument
  | Integer v -> VInt v
  | Boolean b -> VBool b
  | Let (name, e1, e2) ->
    let value = eval env e1 in
    let new_env = Env.add name value env in
    eval new_env e2

and eval_app env abstraction argument =
  let arg = eval env argument in
  let abs = eval env abstraction in
  match abs with
  | VClosure { param; body; env } ->
     let new_env = Env.add param arg env in
     eval new_env body
  | VNative fn -> fn arg
  | _ -> failwith "Invalid function application "

let value_to_string = function
  | VClosure _ -> "VClosure - to_string not implemented"
  | VNative _ -> "VNative - to_string not implemented"
  | VInt v -> string_of_int v
  | VBool b -> string_of_bool b
