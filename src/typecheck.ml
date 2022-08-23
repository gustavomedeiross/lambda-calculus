open Types

type var = string

module Env = Map.Make(String)

type env = Types.typ Env.t

type type_err = TypeError of string

(* TODO: could return a Typedtree after *)
type result = (Types.typ, type_err) Result.t

let (let*) x f = Result.bind x f

let rec typecheck (env : env) (expr : Ast.expr) : result =
  let open Ast in
  match expr with
  | Integer _ -> Ok TInt
  | Boolean _ -> Ok TBool
  | Variable { name } -> begin
    match Env.find_opt name env with
    | None -> Result.error (TypeError "Unbound variable")
    | Some x -> Result.ok x
  end
  | Let (var, e1, e2) -> typecheck_let env var e1 e2
  (* TODO: do we need any sort of "closure" here? *)
  (* like TArrow(typ * typ * env) *)
  | Abstraction (param, typ, body) ->
     let new_env = Env.add param typ env in
     let* body_typ = typecheck new_env body in
     Result.ok (TArrow (typ, body_typ))
  (* TODO: refactor *)
  | Application { abstraction; argument } ->
     let* abs = typecheck env abstraction in
     let* (param_typ, return_typ) = get_arrow_types abs in
     let* arg = typecheck env argument in
     if (Types.equals param_typ arg)
     then
       Result.ok return_typ
     else
       Result.error (TypeError ("Expected " ^ (type_to_string param_typ) ^ ", found " ^ (type_to_string arg)))

and typecheck_let env var e1 e2 =
  let* t1 = typecheck env e1 in
  let new_env = Env.add var t1 env in
  typecheck new_env e2

and get_arrow_types: typ -> (Types.typ * Types.typ, type_err) Result.t = function
  | TArrow (t1, t2) -> Result.ok (t1, t2)
  | typ -> Result.error (TypeError ("Expected abstraction at application, found " ^ type_to_string typ))
 
