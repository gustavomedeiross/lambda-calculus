module EvalTest = struct
  open Alcotest

  let test_eval name ~expr ~output =
    let eval () =
      expr
      |> Lambda.interp
      |> Lambda.Eval.value_to_string
      |> Alcotest.(check (string) "" output)
    in
    Alcotest.test_case name `Quick eval

  let tests = [
    test_eval "Simple binop"
      ~expr:"((fun x -> fun y -> x + y) 2) 4"
      ~output:"6";

    test_eval "id func with boolean "
      ~expr:"(fun x -> x) true"
      ~output:"true";

    test_eval "let expr"
      ~expr:"let x = 2 in x + 5"
      ~output:"7";

    test_eval "let shadows"
      ~expr:"let x = 1 in let x = x + 2 in x"
      ~output:"3";

    test_eval "let with func"
      ~expr:"let f = fun x -> x + 1 in f 10"
      ~output:"11";
  ]
end


let () =
  let open Alcotest in
  run "Lambda tests" [
    "Lambda Eval", EvalTest.tests
  ]
