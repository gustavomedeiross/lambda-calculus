type var = string

module Env = Map.Make(String)

type env = Types.scheme Env.t

type type_err = TypeError of string

(* TODO: could return a Typedtree after *)
type result = (Types.t, type_err) Result.t

let (let*) x f = Result.bind x f

(* TODO: think about a better name *)
type inference_constraint = (Types.t * Types.t)

type substitution = (Types.var * Types.t)

(* TODO: improve this *)
type var_state = VarState of int

let var_of_int (v : int) : Types.var = "?X" ^ string_of_int v

let int_of_var v =
  v
  |> Str.global_replace (Str.regexp "?X") ""
  |> int_of_string

let gen_fresh_var (VarState s) : (Types.var * var_state) =
  let fresh = s + 1 in
  (var_of_int fresh, VarState fresh)

let order_vars vars =
  vars
  |> List.map int_of_var
  |> List.sort Int.compare
  |> List.map var_of_int

type ftv = Types.var list

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let rec ftv_typ (t : Types.t) : Types.var list =
  let open Types in
  match t with
  | TVar v -> [v]
  | TArrow (t1, t2) -> ftv_typ t1 @ ftv_typ t2
  | TInt | TBool -> []

let apply_typ (subst : substitution list) (t : Types.t) : Types.t =
  let open Types in
  let rec apply' subst t =
    let (var, typ) = subst in
    match t with
    | TVar v when String.equal v var -> typ
    | TArrow (t1, t2) -> TArrow (apply' subst t1, apply' subst t2)
    | t -> t
  in
  List.fold_left (fun t s -> apply' s t) t subst

let ftv_scheme (Scheme (vars, typ) : Types.scheme) : ftv =
  diff (ftv_typ typ) vars

let apply_scheme (subst : substitution list) (scheme : Types.scheme) : Types.scheme =
  let Scheme (vars, typ) = scheme in
  (* TODO: some things about this
     1. is it really necessary? why would we try to substitute a generalized variable?
     2. this is not 100% safe (List.remove_assoc removes the first one, not all occurrences of the key)
   *)
  let subst' = List.fold_left (fun s v -> List.remove_assoc v s) subst vars in
  Scheme (vars, apply_typ subst' typ)

let ftv_env (env : env) : ftv =
  Env.fold (fun _var scheme acc -> acc @ ftv_scheme scheme) env []

let apply_env (subst : substitution list) (env : env) : env =
  env |> Env.map (fun scheme -> apply_scheme subst scheme)

let variable_does_not_appear_in (var : Types.var) (t : Types.t) : bool =
  t |> ftv_typ |> List.mem var |> not

let apply_constraint (subst : substitution list) (constr : inference_constraint) : inference_constraint =
  let (t1, t2) = constr in
  (apply_typ subst t1, apply_typ subst t2)

let apply_constraints subst cs = List.map (fun c -> apply_constraint subst c) cs

(* TODO: check if this is correct *)
(* also called mgu - most general unifier *)
let rec unify (constraints : inference_constraint list) : (substitution list, type_err) Result.t =
  match constraints with
  | [] -> Ok []
  | (s, t) :: cs when Types.equals s t -> unify cs
  | (TArrow (t1, t2), TArrow (t3, t4)) :: cs ->
     let constraints = (t1, t3) :: (t2, t4) :: cs in
     unify constraints
  | (TVar v, t) :: cs when variable_does_not_appear_in v t ->
     var_bind cs v t
  | (t, TVar v) :: cs when variable_does_not_appear_in v t ->
     var_bind cs v t
  | _ -> Result.error (TypeError "Unification error")

and var_bind (cs : inference_constraint list) (v : Types.var) (t : Types.t) : (substitution list, type_err) Result.t =
  let subst = (v, t) in
  let cs = apply_constraints [subst] cs in
  unify cs |> Result.map (fun substs -> subst :: substs)

let replace_var (st : var_state) (tv : Types.var) (typ : Types.t) : (Types.t * var_state) =
  let open Types in
  let (var, st) = gen_fresh_var st in
  let subst = (tv, TVar var) in
  let typ = apply_typ [subst] typ in
  (typ, st)

let instantiate (st : var_state) (scheme : Types.scheme) : (Types.t * var_state) =
  let open Types in
  let Scheme(vars, typ)  = scheme in
  vars |> List.fold_left (fun (typ, st) v -> replace_var st v typ) (typ, st)

let generalize (cs : inference_constraint list) (env : env) (var : (Types.var * Types.t)) : ((Types.scheme * env), type_err) Result.t =
  let open Types in
  let generalize' env typ =
    let vars = diff (ftv_typ typ) (ftv_env env) in
    Scheme (vars, typ)
  in
  let (_var, typ) = var in
  let* subst = unify cs in
  let principal_type = apply_typ subst typ in
  let env = apply_env subst env in
  let scheme = generalize' env principal_type in
  Result.ok (scheme, env)

let dont_generalize (typ : Types.t) : Types.scheme = Types.Scheme ([], typ)

(* TODO: refactor this function *)
let rec infer (env : env) (var_state : var_state) (expr : Ast.expr) : (Types.t * var_state * inference_constraint list, type_err) Result.t =
  let open Ast in
  let open Types in
  match expr with
  | Integer _ -> Ok (TInt, var_state, [])
  | Boolean _ -> Ok (TBool, var_state, [])
  | Variable { name } -> begin
    match Env.find_opt name env with
    | None -> Error (TypeError "Unbound variable")
    | Some ts ->
       let (t, var_state) = instantiate var_state ts in
       Ok (t, var_state, [])
  end
  (* TODO: use type annotation *)
  | Abstraction (param, Some typ, body) -> begin
      let (var, var_state) = gen_fresh_var var_state in
      let param_typ = TVar var in
      let env = Env.add param (dont_generalize param_typ) env in
      let* (body_typ, var_state, constraints) = infer env var_state body in
      let constraints = (param_typ, typ) :: constraints in
      Ok ((TArrow (param_typ, body_typ)), var_state, constraints)
  end
  | Abstraction (param, None, body) -> begin
      let (var, var_state) = gen_fresh_var var_state in
      let param_typ = TVar var in
      let env = Env.add param (dont_generalize param_typ) env in
      let* (body_typ, var_state, constraints) = infer env var_state body in
      Ok ((TArrow (param_typ, body_typ)), var_state, constraints)
  end
  | Application { abstraction; argument } ->
     let* (abs_typ, var_state, abs_constraints) = infer env var_state abstraction in
     let* (arg_typ, var_state, arg_constraints) = infer env var_state argument in
     let (var, var_state) = gen_fresh_var var_state in
     let application_constraint = (abs_typ, TArrow (arg_typ, (TVar var))) in
     let constraints = application_constraint :: abs_constraints @ arg_constraints in
     Ok (TVar var, var_state, constraints)
  | Let (var, e1, e2) ->
     let* (t1, var_state, e1_constraints) = infer env var_state e1 in
     let* (scheme, env) = generalize e1_constraints env (var, t1) in
     let env = Env.add var scheme env in
     let* (t2, var_state, e2_constraints) = infer env var_state e2 in
     Ok (t2, var_state, e1_constraints @ e2_constraints)

let remove_duplicates eq lst =
  List.fold_left (fun acc x ->
      let exists = List.exists (fun v -> eq v x) acc in
      if exists then
        acc
      else
        x :: acc
    ) [] lst
  |> List.rev

let reset_type_variables typ =
  let subst = typ
    |> ftv_typ
    |> order_vars
    |> remove_duplicates String.equal
    |> List.mapi (fun pos v -> v, Types.TVar (var_of_int (pos+1)))
  in
  apply_typ subst typ

let typecheck (env : env) (expr : Ast.expr) : result =
  let* (typ, _, constraints) = infer env (VarState 0) expr in
  let* subst = unify constraints in
  (* let principal_type = List.fold_left (fun t s -> apply s t) typ subst in *)
  let principal_type = apply_typ subst typ in
  Result.ok (reset_type_variables principal_type)
