module Eval = Eval
module Typecheck = Typecheck
module Types = Types

let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let eval expr = Eval.eval Eval.Env.empty expr

let typecheck s =
  s |> parse |> Typecheck.typecheck Typecheck.Env.empty

let interp_typecheck expr =
  match Typecheck.typecheck Typecheck.Env.empty expr with
  | Error TypeError e -> failwith e
  | Ok _ -> expr

let interp (s : string) : Eval.value =
  s |> parse |> interp_typecheck |> eval
