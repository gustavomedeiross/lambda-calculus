type var = string

module Env = Map.Make(String)

type env = Types.t Env.t

type type_err = TypeError of string

(* TODO: could return a Typedtree after *)
type result = (Types.t, type_err) Result.t

let (let*) x f = Result.bind x f

type inference_constraint = (Types.t * Types.t)

type substitution = (Types.var * Types.t)

(* TODO: improve this *)
type var_state = VarState of int

let gen_fresh_var (VarState s) : (Types.var * var_state) =
  let fresh = s + 1 in
  ("?X" ^ string_of_int fresh, VarState fresh)

(* TODO: refactor this function *)
let rec type_reconstruct (env : env) (var_state : var_state) (expr : Ast.expr) : (Types.t * var_state * inference_constraint list, type_err) Result.t =
  let open Ast in
  let open Types in
  match expr with
  | Integer _ -> Ok (TInt, var_state, [])
  | Boolean _ -> Ok (TBool, var_state, [])
  | Variable { name } -> begin
    match Env.find_opt name env with
    | None -> Error (TypeError "Unbound variable")
    | Some t -> Ok (t, var_state, [])
  end
  (* TODO: use type annotation *)
  | Abstraction (param, Some typ, body) -> begin
      let (var, var_state) = gen_fresh_var var_state in
      let param_typ = TVar var in
      let env = Env.add param param_typ env in
      let* (body_typ, var_state, constraints) = type_reconstruct env var_state body in
      let constraints = (param_typ, typ) :: constraints in
      Ok ((TArrow (param_typ, body_typ)), var_state, constraints)
  end
  | Abstraction (param, None, body) -> begin
      let (var, var_state) = gen_fresh_var var_state in
      let param_typ = TVar var in
      let env = Env.add param param_typ env in
      let* (body_typ, var_state, constraints) = type_reconstruct env var_state body in
      Ok ((TArrow (param_typ, body_typ)), var_state, constraints)
  end
  | Application { abstraction; argument } ->
     let* (abs_typ, var_state, abs_constraints) = type_reconstruct env var_state abstraction in
     let* (arg_typ, var_state, arg_constraints) = type_reconstruct env var_state argument in
     let (var, var_state) = gen_fresh_var var_state in
     let application_constraint = (abs_typ, TArrow (arg_typ, (TVar var))) in
     let constraints = application_constraint :: abs_constraints @ arg_constraints in
     Ok (TVar var, var_state, constraints)
  (* TODO: let-polymorphism *)
  | Let (var, e1, e2) ->
     let* (t1, var_state, e1_constraints) = type_reconstruct env var_state e1 in
     let env = Env.add var t1 env in
     let* (t2, var_state, e2_constraints) = type_reconstruct env var_state e2 in
     Ok (t2, var_state, e1_constraints @ e2_constraints)

let rec variables_in (t : Types.t) : Types.var list =
  let open Types in
  match t with
  | TVar v -> [v]
  | TArrow (t1, t2) -> variables_in t1 @ variables_in t2
  | _ -> []

let variable_does_not_appear_in (var : Types.var) (t : Types.t) : bool =
  t |> variables_in |> List.mem var |> not

let rec apply (subst : substitution) (t : Types.t) : Types.t =
  let open Types in
  let (var, typ) = subst in
  match t with
  | TVar v when v == var -> typ
  | TArrow (t1, t2) -> TArrow (apply subst t1, apply subst t2)
  | t -> t

let substitute_constraint_list (constraints : inference_constraint list) (subst : substitution) : inference_constraint list =
  List.map (fun (c1, c2) -> ((apply subst c1), (apply subst c2))) constraints

(* TODO: check if this is correct *)
let rec unify (constraints : inference_constraint list) : (substitution list, type_err) Result.t =
  match constraints with
  | [] -> Ok []
  | (s, t) :: cs when Types.equals s t -> unify cs
  | (TArrow (t1, t2), TArrow (t3, t4)) :: cs ->
     let constraints = (t1, t3) :: (t2, t4) :: cs in
     unify constraints
  | (TVar v, t) :: cs when variable_does_not_appear_in v t ->
     let substitution = (v, t) in
     let cs = substitute_constraint_list cs substitution in
     unify cs |> Result.map (fun substitutions -> substitution :: substitutions)
  | (t, TVar v) :: cs when variable_does_not_appear_in v t ->
     let substitution = (v, t) in
     let cs = substitute_constraint_list cs substitution in
     unify cs |> Result.map (fun substitutions -> substitution :: substitutions)
  | _ -> Result.error (TypeError "Unification error")

let typecheck (env : env) (expr : Ast.expr) : result =
  let* (t, _, constraints) = type_reconstruct env (VarState 0) expr in
  let* substitutions = unify constraints in
  let principal_type = List.fold_left (fun t s -> apply s t) t substitutions in
  Result.ok (principal_type)
