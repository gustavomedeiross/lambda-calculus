open Ast
open Types

type var = string
type typeenv = (var * Types.typ) list

type type_err = TypeError of string

(* TODO: could return a Typedtree after *)
type result = (Types.typ, type_err) Result.t

let rec typecheck (env : typeenv) (expr : Ast.expr) : result =
  let open Ast in
  match expr with
  | Integer _ -> Ok TInteger
  | Boolean _ -> Ok TBool
  | BinOp (op, e1, e2) -> typecheck_binop env op e1 e2
  | _ -> failwith "Not implemented"

and typecheck_binop env op e1 e2 =
  let t1 = unsafe_typecheck env e1 in
  let t2 = unsafe_typecheck env e2 in
  match op with
  | Plus -> typecheck_int_binop t1 t2

and typecheck_int_binop t1 t2 =
  match (t1, t2) with
  | (TInteger, TInteger) -> Result.ok TInteger
  | (t, TInteger) | (TInteger, t) -> binop_error t
  | (t1, _) -> binop_error t1

and binop_error t =
  let t = Types.type_to_string t in
  Result.error (TypeError ("Expected int, found " ^ t))

(* TODO: remove this function and use a monad *)
and unsafe_typecheck env expr =
  match typecheck env expr with
  | Ok t -> t
  | Error (TypeError e) -> failwith e
 
