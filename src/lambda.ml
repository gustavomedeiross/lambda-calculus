module Eval = Eval
module Typecheck = Typecheck
module Types = Types

let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let typecheck expr =
  match Typecheck.typecheck [] expr with
  | Error TypeError e -> failwith e
  | Ok _ -> expr

let eval expr = Eval.eval Eval.Env.empty expr

let interp (s : string) : Eval.value =
  s |> parse |> typecheck |> eval
