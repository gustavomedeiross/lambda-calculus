let test_eval str ~expr ~output =
  expr
  |> Lambda.interp
  |> Lambda.Eval.value_to_string
  |> Alcotest.(check (string) str output)

let simple_binop () =
  test_eval "simple_binop"
    ~expr:"((fun x -> fun y -> x + y) 2) 4"
    ~output:"6"

let id_func_with_boolean () =
  test_eval "id_func_with_boolean"
    ~expr:"(fun x -> x) true"
    ~output:"true"

let let_expr () =
  test_eval "let expr"
    ~expr:"let x = 2 in x + 5"
    ~output:"7"

let let_shadows () =
  test_eval "let shadows"
    ~expr:"let x = 1 in let x = x + 2 in x"
    ~output:"3"

let let_with_func () =
  test_eval "let shadows"
    ~expr:"let f = fun x -> x + 1 in f 10"
    ~output:"11"

let () =
  let open Alcotest in
  run "Lambda tests" [
    "Lambda Eval", [
        test_case "Simple expression" `Quick simple_binop
      ; test_case "id func with boolean " `Quick id_func_with_boolean
      ; test_case "let expr" `Quick let_expr
      ; test_case "let shadows" `Quick let_shadows
      ; test_case "let with func" `Quick let_with_func
    ]
  ]
