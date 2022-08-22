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
      ~expr:"((fun x : int -> fun y : int -> x + y) 2) 4"
      ~output:"6";

    test_eval "id func with bool "
      ~expr:"(fun x : bool -> x) true"
      ~output:"true";

    test_eval "let expr"
      ~expr:"let x = 2 in x + 5"
      ~output:"7";

    test_eval "let shadows"
      ~expr:"let x = 1 in let x = x + 2 in x"
      ~output:"3";

    test_eval "let with func"
      ~expr:"let f = fun x : int -> x + 1 in f 10"
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

    typechecks_to "binop typechecks"
      ~expr:"2 + 2"
      ~typ:Types.TInt;

    typecheck_fails_with "binop with one wrong type"
      ~expr:"2 + true"
      ~error:"Expected int, found bool";

    typecheck_fails_with "binop with two wrong types"
      ~expr:"false + true"
      ~error:"Expected int, found bool";

    typechecks_to "basic let expr"
      ~expr:"let x = 2 in x + 3"
      ~typ:Types.TInt;

    typecheck_fails_with "invalid binop with let"
      ~expr:"let x = true in x + 2"
      ~error:"Expected int, found bool";

    typechecks_to "abstraction with type signature"
      ~expr:"fun x : int -> x + 2"
      ~typ:(Types.TArrow (Types.TInt, Types.TInt));

    typechecks_to "abstraction with (bool -> int) signature"
      ~expr:"fun x : bool -> 2"
      ~typ:(Types.TArrow (Types.TBool, Types.TInt));

    typechecks_to "nested abstractions"
      ~expr:"(fun x : int -> (fun y : int -> x + y))"
      ~typ:(Types.TArrow (Types.TArrow (Types.TInt, Types.TInt), Types.TInt));

    typechecks_to "application with valid type"
      ~expr:"(fun x : int -> x + 2) 2"
      ~typ:(TInt);

    typecheck_fails_with "application with invalid parameter"
      ~expr:"(fun x : int -> x + 2) true"
      ~error:"Expected int, found bool";

    typechecks_to "returns curried abstraction type"
      ~expr:"(fun x : int -> (fun y : int -> x + y)) 2"
      ~typ:(TArrow (TInt, TInt));

    typechecks_to "nested abstractions with let"
      ~expr:"let f = fun x : int -> (fun y : int -> x + y) in f"
      ~typ:(Types.TArrow (Types.TArrow (Types.TInt, Types.TInt), Types.TInt));

    typechecks_to "complete application of nested abstractions"
      ~expr:"let f = fun x : int -> (fun y : int -> x + y) in f 5 10"
      ~typ:(Types.TInt);
  ]
end

let () =
  Alcotest.run "Lambda tests" [
    "Lambda Eval", EvalTest.tests;
    "Lambda Typechecking", TypecheckTest.tests;
  ]
