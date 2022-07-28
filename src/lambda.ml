let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let interp (s : string) : Eval.value =
  s |> parse |> Eval.eval []

let () =
  let program = "((fun x -> fun y -> x + y) 2) 4" in
  let output = interp program in
  print_endline (Eval.value_to_string output);
