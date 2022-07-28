let test_eval str ~expr ~output =
  expr
  |> Lambda.interp
  |> Lambda.Eval.value_to_string
  |> Alcotest.(check (string) str output)

let simple_expression () =
  test_eval
    "simple_expression"
    ~expr:"((fun x -> fun y -> x + y) 2) 4"
    ~output:"6"

let () =
  let open Alcotest in
  run "Lambda tests" [
    "Lambda Eval", [
      test_case "Simple expression" `Quick simple_expression
    ]
  ]
