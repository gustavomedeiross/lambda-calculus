let test_eval str ~expr ~output =
  expr
  |> Lambda.interp
  |> Lambda.Eval.value_to_string
  |> Alcotest.(check (string) str output)

let simple_binop () =
  test_eval
    "simple_binop"
    ~expr:"((fun x -> fun y -> x + y) 2) 4"
    ~output:"6"

let id_func_with_boolean () =
  test_eval
    "id_func_with_boolean"
    ~expr:"(fun x -> x) true"
    ~output:"true"

let () =
  let open Alcotest in
  run "Lambda tests" [
    "Lambda Eval", [
      test_case "Simple expression" `Quick simple_binop;
      test_case "id func with boolean " `Quick id_func_with_boolean
    ]
  ]
