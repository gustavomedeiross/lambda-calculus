open Types

module Env = Map.Make(String)

type env = scheme Env.t

type type_err = TypeError of string

type result = (typ, type_err) Result.t

let (let*) x f = Result.bind x f

type constr = (typ * typ)

type substitution = (var * typ)

type ftv = var list

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let rec ftv_typ : typ -> var list = function
  | TVar v -> [v]
  | TArrow (t1, t2) -> ftv_typ t1 @ ftv_typ t2
  | TInt | TBool -> []

let apply_typ (subst : substitution list) (t : typ) : typ =
  let rec apply' subst t =
    let (var, typ) = subst in
    match t with
    | TVar v when TyVar.equal v var -> typ
    | TArrow (t1, t2) -> TArrow (apply' subst t1, apply' subst t2)
    | t -> t
  in
  List.fold_left (fun t s -> apply' s t) t subst

let ftv_scheme (Scheme (vars, typ) : scheme) : ftv =
  diff (ftv_typ typ) vars

let apply_scheme (subst : substitution list) (scheme : scheme) : scheme =
  let Scheme (vars, typ) = scheme in
  Scheme (vars, apply_typ subst typ)

let ftv_env (env : env) : ftv =
  Env.fold (fun _var scheme acc -> acc @ ftv_scheme scheme) env []

let apply_env (subst : substitution list) (env : env) : env =
  env |> Env.map (fun scheme -> apply_scheme subst scheme)

let variable_does_not_appear_in (var : var) (t : typ) : bool =
  t |> ftv_typ |> List.mem var |> not

let apply_constraint (subst : substitution list) (c : constr) : constr =
  let (t1, t2) = c in
  (apply_typ subst t1, apply_typ subst t2)

let apply_constraints subst cs = List.map (fun c -> apply_constraint subst c) cs

(* also called mgu - most general unifier *)
let rec unify (constraints : constr list) : (substitution list, type_err) Result.t =
  match constraints with
  | [] -> Ok []
  | (s, t) :: cs when Types.equal s t -> unify cs
  | (TArrow (t1, t2), TArrow (t3, t4)) :: cs ->
     let constraints = (t1, t3) :: (t2, t4) :: cs in
     unify constraints
  | (TVar v, t) :: cs when variable_does_not_appear_in v t ->
     var_bind cs v t
  | (t, TVar v) :: cs when variable_does_not_appear_in v t ->
     var_bind cs v t
  | _ -> Result.error (TypeError "Unification error")

and var_bind (cs : constr list) (v : var) (t : typ) : (substitution list, type_err) Result.t =
  let subst = (v, t) in
  let cs = apply_constraints [subst] cs in
  unify cs |> Result.map (fun substs -> subst :: substs)

let replace_var (st : TyVar.state) (tv : var) (typ : typ) : (typ * TyVar.state) =
  let (var, st) = TyVar.gen_fresh st in
  let subst = (tv, TVar var) in
  let typ = apply_typ [subst] typ in
  (typ, st)

let instantiate (st : TyVar.state) (scheme : scheme) : (typ * TyVar.state) =
  let Scheme(vars, typ)  = scheme in
  vars |> List.fold_left (fun (typ, st) v -> replace_var st v typ) (typ, st)

let generalize (cs : constr list) (env : env) (var : (string * typ)) : ((scheme * env), type_err) Result.t =
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

let dont_generalize (typ : typ) : scheme = Scheme ([], typ)

let rec infer (env : env) (var_st : TyVar.state) (expr : Ast.expr) : (typ * TyVar.state * constr list, type_err) Result.t =
  let open Ast in
  match expr with
  | Integer _ -> Ok (TInt, var_st, [])
  | Boolean _ -> Ok (TBool, var_st, [])
  | Variable { name } -> begin
    match Env.find_opt name env with
    | None -> Error (TypeError "Unbound variable")
    | Some ts ->
       let (t, var_st) = instantiate var_st ts in
       Ok (t, var_st, [])
  end
  | Abstraction (param, Some typ, body) -> begin
      let (var, var_st) = TyVar.gen_fresh var_st in
      let param_typ = TVar var in
      let env = Env.add param (dont_generalize param_typ) env in
      let* (body_typ, var_st, constraints) = infer env var_st body in
      let constraints = (param_typ, typ) :: constraints in
      Ok ((TArrow (param_typ, body_typ)), var_st, constraints)
  end
  | Abstraction (param, None, body) -> begin
      let (var, var_st) = TyVar.gen_fresh var_st in
      let param_typ = TVar var in
      let env = Env.add param (dont_generalize param_typ) env in
      let* (body_typ, var_st, constraints) = infer env var_st body in
      Ok ((TArrow (param_typ, body_typ)), var_st, constraints)
  end
  | Application { abstraction; argument } ->
     let* (abs_typ, var_st, abs_constraints) = infer env var_st abstraction in
     let* (arg_typ, var_st, arg_constraints) = infer env var_st argument in
     let (var, var_st) = TyVar.gen_fresh var_st in
     let application_constraint = (abs_typ, TArrow (arg_typ, (TVar var))) in
     let constraints = application_constraint :: abs_constraints @ arg_constraints in
     Ok (TVar var, var_st, constraints)
  | Let (var, e1, e2) ->
     let* (t1, var_st, e1_constraints) = infer env var_st e1 in
     let* (scheme, env) = generalize e1_constraints env (var, t1) in
     let env = Env.add var scheme env in
     let* (t2, var_st, e2_constraints) = infer env var_st e2 in
     Ok (t2, var_st, e1_constraints @ e2_constraints)

let remove_duplicates eq lst =
  List.fold_left (fun acc x ->
      let exists = List.exists (fun v -> eq v x) acc in
      if exists then
        acc
      else
        x :: acc
    ) [] lst
  |> List.rev

let order_vars vars =
  vars
  |> List.sort TyVar.compare

let reset_type_variables typ =
  let subst = typ
    |> ftv_typ
    |> order_vars
    |> remove_duplicates TyVar.equal
    |> List.fold_left (fun (acc, state) v ->
           let (var', state') = TyVar.gen_fresh state in
           let subst = (v, TVar var') in
           (subst :: acc, state')
         ) ([], TyVar.initial_state ())
    |> fst
  in
  apply_typ subst typ

let typecheck (env : env) (expr : Ast.expr) : result =
  let* (typ, _, constraints) = infer env (TyVar.initial_state ()) expr in
  let* subst = unify constraints in
  let principal_type = apply_typ subst typ in
  Result.ok (reset_type_variables principal_type)
