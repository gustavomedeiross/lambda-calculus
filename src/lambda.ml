module Eval = Eval
module Typecheck = Typecheck
module Types = Types

let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

type native =
  {
    name : string;
    typ : Types.t;
    fn : (Eval.value -> Eval.value);
  }

let initial_type_env =
  Typecheck.Env.empty
  |> Native.add_functions_to_typ_env

let initial_eval_env =
  Eval.Env.empty
  |> Native.add_functions_to_eval_env

let eval expr =
  Eval.eval initial_eval_env expr

let typecheck s =
  s |> parse |> Typecheck.typecheck initial_type_env

let interp_typecheck expr =
  match Typecheck.typecheck initial_type_env expr with
  | Error TypeError e -> failwith e
  | Ok _ -> expr

let interp (s : string) : Eval.value =
  s |> parse |> interp_typecheck |> eval
