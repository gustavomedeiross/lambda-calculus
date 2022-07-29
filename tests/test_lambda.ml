module EvalTest = struct
  let test_eval name ~expr ~output =
    let eval () =
      expr
      |> Lambda.interp
      |> Lambda.Eval.value_to_string
      |> Alcotest.(check (string) "" output)
    in
    Alcotest.test_case name `Quick eval

  let tests = [
    test_eval "simple binop"
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

module TypecheckTest = struct
  module Types = Lambda.Types
  module Typecheck = Lambda.Typecheck

  (* TODO: implement testable inteface for Types *)
  let typechecks_to name ~expr ~typ =
    let typecheck () =
      let expected = Types.type_to_string typ in
      let actual =
        expr
        |> Lambda.parse
        |> Typecheck.typecheck []
        |> Result.get_ok
        |> Types.type_to_string
      in
      Alcotest.(check (string) "" expected) actual
    in
    Alcotest.test_case name `Quick typecheck

  let typecheck_fails_with name ~expr ~error =
    let typecheck () =
      expr
      |> Lambda.parse
      |> Typecheck.typecheck []
      |> Result.get_error
      |> fun (Typecheck.TypeError e) -> e
      |> Alcotest.(check (string) "" error)
    in
    Alcotest.test_case name `Quick typecheck

  let tests = [
    typechecks_to "integer typechecks"
      ~expr:"1"
      ~typ:Types.TInteger;

    typechecks_to "boolean typechecks"
      ~expr:"true"
      ~typ:Types.TBool;

    typechecks_to "binop typechecks"
      ~expr:"2 + 2"
      ~typ:Types.TInteger;

    typecheck_fails_with "binop with one wrong type"
      ~expr:"2 + true"
      ~error:"Expected int, found boolean";

    typecheck_fails_with "binop with two wrong types"
      ~expr:"false + true"
      ~error:"Expected int, found boolean";

    (* typechecks_to "Basic let"
     *   ~expr:"let x : int = 1 in x"
     *   ~typ:"int"; *)

    (* typecheck_fails_with "Invalid let"
     *   ~expr:"let x : int = true in x"
     *   ~error:"Found boolean, expected int"; *)
  ]
end

let () =
  Alcotest.run "Lambda tests" [
    "Lambda Eval", EvalTest.tests;
    "Lambda Typechecking", TypecheckTest.tests;
  ]
