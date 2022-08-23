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
    test_eval "simple native function"
      ~expr:"((fun x : int -> fun y : int -> plus x y) 2) 4"
      ~output:"6";

    test_eval "id func with bool "
      ~expr:"(fun x : bool -> x) true"
      ~output:"true";

    test_eval "let expr"
      ~expr:"let x = 2 in plus x 5"
      ~output:"7";

    test_eval "let shadows"
      ~expr:"let x = 1 in let x = plus x 2 in x"
      ~output:"3";

    test_eval "let with func"
      ~expr:"let f = fun x : int -> plus x 1 in f 10"
      ~output:"11";

    test_eval "native function"
      ~expr:"plus 2 2"
      ~output:"4";
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
        |> Lambda.typecheck
        |> Result.get_ok
        |> Types.type_to_string
      in
      Alcotest.(check (string) "" expected) actual
    in
    Alcotest.test_case name `Quick typecheck

  let typecheck_fails_with name ~expr ~error =
    let typecheck () =
      expr
      |> Lambda.typecheck
      |> Result.get_error
      |> fun (Typecheck.TypeError e) -> e
      |> Alcotest.(check (string) "" error)
    in
    Alcotest.test_case name `Quick typecheck

  let tests = [
    typechecks_to "integer typechecks"
      ~expr:"1"
      ~typ:Types.TInt;

    typechecks_to "boolean typechecks"
      ~expr:"true"
      ~typ:Types.TBool;

    typechecks_to "native function"
      ~expr:"plus 1 3"
      ~typ:(Types.TInt);

    typecheck_fails_with "native with one wrong type"
      ~expr:"plus 2 true"
      ~error:"Expected int, found bool";

    typecheck_fails_with "native with two wrong types"
      ~expr:"plus false true"
      ~error:"Expected int, found bool";

    typechecks_to "basic let expr"
      ~expr:"let x = 2 in plus x 3"
      ~typ:Types.TInt;

    typecheck_fails_with "invalid native with let"
      ~expr:"let x = true in plus x 2"
      ~error:"Expected int, found bool";

    typechecks_to "abstraction with type signature"
      ~expr:"fun x : int -> plus x 2"
      ~typ:(Types.TArrow (Types.TInt, Types.TInt));

    typechecks_to "abstraction with (bool -> int) signature"
      ~expr:"fun x : bool -> 2"
      ~typ:(Types.TArrow (Types.TBool, Types.TInt));

    typechecks_to "nested abstractions"
      ~expr:"(fun x : int -> (fun y : int -> plus x y))"
      ~typ:(Types.TArrow (Types.TArrow (Types.TInt, Types.TInt), Types.TInt));

    typechecks_to "application with valid type"
      ~expr:"(fun x : int -> plus x 2) 2"
      ~typ:(TInt);

    typecheck_fails_with "application with invalid parameter"
      ~expr:"(fun x : int -> plus x 2) true"
      ~error:"Expected int, found bool";

    typechecks_to "returns curried abstraction type"
      ~expr:"(fun x : int -> (fun y : int -> plus x y)) 2"
      ~typ:(TArrow (TInt, TInt));

    typechecks_to "nested abstractions with let"
      ~expr:"let f = fun x : int -> (fun y : int -> plus x y) in f"
      ~typ:(Types.TArrow (Types.TArrow (Types.TInt, Types.TInt), Types.TInt));

    typechecks_to "complete application of nested abstractions"
      ~expr:"let f = fun x : int -> (fun y : int -> plus x y) in f 5 10"
      ~typ:(Types.TInt);

    typechecks_to "partial application of native function"
      ~expr:"plus 1"
      ~typ:(Types.TArrow (Types.TInt, Types.TInt));
  ]
end

let () =
  Alcotest.run "Lambda tests" [
    "Lambda Eval", EvalTest.tests;
    "Lambda Typechecking", TypecheckTest.tests;
  ]
