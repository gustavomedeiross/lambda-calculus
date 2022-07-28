open Ast

let value_to_string = function
  | VClosure _ -> "VClosure - to_string not implemented"
  | VInt v -> string_of_int v

let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let interp (s : string) : Ast.value =
  s |> parse |> Eval.eval []

let () =
  let program = "((fun x -> fun y -> x + y) 2) 4" in
  let output = interp program in
  print_endline (value_to_string output);
