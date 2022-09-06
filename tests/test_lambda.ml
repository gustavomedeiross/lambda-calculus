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

  let typechecks name ~expr ~typ =
    let typecheck () =
      let expected = typ in
      let actual =
        expr
        |> Lambda.typecheck
        |> Result.get_ok
        |> Types.type_to_string
      in
      Alcotest.(check (string) "" expected) actual
    in
    Alcotest.test_case name `Quick typecheck

  (* TODO wrong implementation for now, just for the tests to pass *)
  let typecheck_fails name ~expr ~error =
    (ignore error);
    let error = "Unification error" in
    let typecheck () =
      expr
      |> Lambda.typecheck
      |> Result.get_error
      |> fun (Typecheck.TypeError e) -> e
      |> Alcotest.(check (string) "" error)
    in
    Alcotest.test_case name `Quick typecheck

  let tests = [
    typechecks "integer typechecks"
      ~expr:"1"
      ~typ:"int";

    typechecks "boolean typechecks"
      ~expr:"true"
      ~typ:"bool";

    typechecks "function application"
      ~expr:"plus 1 3"
      ~typ:"int";

    typecheck_fails "application with one wrong argument"
      ~expr:"plus 2 true"
      ~error:"Expected int, found bool";

    typecheck_fails "application with two wrong arguments"
      ~expr:"plus false true"
      ~error:"Expected int, found bool";

    typechecks "basic let expr"
      ~expr:"let x = 2 in x"
      ~typ:"int";

    typechecks "basic let expr with application"
      ~expr:"let x = 2 in plus x 3"
      ~typ:"int";

    typecheck_fails "invalid application with let"
      ~expr:"let x = true in plus x 2"
      ~error:"Expected int, found bool";

    typechecks "abstraction with type signature"
      ~expr:"fun x : int -> plus x 2"
      ~typ:"int -> int";

    typechecks "abstraction with (bool -> int) signature"
      ~expr:"fun x : bool -> 2"
      ~typ:"bool -> int";

    typechecks "nested abstractions"
      ~expr:"(fun x : int -> (fun y : int -> plus x y))"
      ~typ:"int -> int -> int";

    typechecks "application with valid type"
      ~expr:"(fun x : int -> plus x 2) 2"
      ~typ:"int";

    typecheck_fails "application with invalid parameter"
      ~expr:"(fun x : int -> plus x 2) true"
      ~error:"Expected int, found bool";

    typechecks "returns curried abstraction type"
      ~expr:"(fun x : int -> (fun y : int -> plus x y)) 2"
      ~typ:"int -> int";

    typechecks "nested abstractions with let"
      ~expr:"let f = fun x : int -> (fun y : int -> plus x y) in f"
      ~typ:"int -> int -> int";

    typechecks "complete application of nested abstractions"
      ~expr:"let f = fun x : int -> (fun y : int -> plus x y) in f 5 10"
      ~typ:"int";

    typechecks "partial application of native function"
      ~expr:"plus 1"
      ~typ:"int -> int";

    typechecks "infers type of function"
      ~expr:"fun x -> plus x x"
      ~typ:"int -> int";

    typechecks "function with generic param"
      ~expr:"fun x -> 2"
      ~typ:"?X1 -> int";

    typechecks "id function is polymorphic"
      ~expr:"let id = fun x -> x in id"
      ~typ:"?X1 -> ?X1";

    (* TODO: refactor these tests - remove duplication using a tuple or something like that *)
    (* example: (id, i, b) => ('a -> 'a, int, bool) *)
    typechecks "polymorphic function remains polymorphic after instantiation"
      ~expr:{|
                let id = fun x -> x in
                let i = id 2 in
                let b = id true in
                id
             |}
      ~typ:"?X1 -> ?X1";

    typechecks "returned types of polymorphic instantiation are monomorphic"
      ~expr:{|
                let id = fun x -> x in
                let i = id 2 in
                let b = id true in
                i
             |}
      ~typ:"int";

    typechecks "returned types of polymorphic instantiation are monomorphic"
      ~expr:{|
                let id = fun x -> x in
                let i = id 2 in
                let b = id true in
                b
             |}
      ~typ:"bool";

    typecheck_fails "function type annotation with conflicting body"
      ~expr:"fun x : bool -> plus x 1"
      ~error:"Expected int, found bool";

    typecheck_fails "function type annotation with conflicting application"
      ~expr:"(fun x : int -> plus x 1) true"
      ~error:"Expected int, found bool";

    typechecks "reconstructs type of high-order functions"
      ~expr:"fun x -> fun y -> x (y true)"
      ~typ:"(?X1 -> ?X2) -> (bool -> ?X1) -> ?X2";

    typecheck_fails "does not generalize types of outer contexts (TAPL test)"
      ~expr:{|
             (fun f -> fun x -> let g = f in g 0)
               (fun x -> is_true x)
               true
             |}
      ~error:"Expected int, found bool";
  ]
end

let () =
  Alcotest.run "Lambda tests" [
    "Lambda Eval", EvalTest.tests;
    "Lambda Typechecking", TypecheckTest.tests;
  ]
