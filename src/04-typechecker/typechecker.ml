module Error = Utils.Error
module Ast = Language.Ast
module Tau = Language.Tau.TauImpl
module Const = Language.Const
module Context = Language.Context
module Exception = Language.Exception
module PrettyPrint = Language.PrettyPrint

module ContextHolderModule =
  Context.Make (Ast.Variable) (Map.Make (Ast.Variable)) (Tau)

type state = {
  variables :
    (Ast.ty_param list * Ast.tau_param list * Tau.t Ast.ty)
    ContextHolderModule.t;
  type_definitions : (Ast.ty_param list * Tau.t Ast.ty_def) Ast.TyNameMap.t;
  op_signatures : (Tau.t Ast.ty * Tau.t Ast.ty * Tau.t Ast.tau) Ast.OpNameMap.t;
}

let initial_state =
  {
    variables = ContextHolderModule.empty;
    type_definitions =
      (Ast.TyNameMap.empty
      |> Ast.TyNameMap.add Ast.bool_ty_name
           ([], Ast.TyInline (Ast.TyConst Const.BooleanTy))
      |> Ast.TyNameMap.add Ast.int_ty_name
           ([], Ast.TyInline (Ast.TyConst Const.IntegerTy))
      |> Ast.TyNameMap.add Ast.unit_ty_name ([], Ast.TyInline (Ast.TyTuple []))
      |> Ast.TyNameMap.add Ast.string_ty_name
           ([], Ast.TyInline (Ast.TyConst Const.StringTy))
      |> Ast.TyNameMap.add Ast.float_ty_name
           ([], Ast.TyInline (Ast.TyConst Const.FloatTy))
      |> Ast.TyNameMap.add Ast.empty_ty_name ([], Ast.TySum [])
      |>
      let a = Ast.TyParamModule.fresh "list" in
      Ast.TyNameMap.add Ast.list_ty_name
        ( [ a ],
          Ast.TySum
            [
              (Ast.nil_label, None);
              ( Ast.cons_label,
                Some
                  (Ast.TyTuple
                     [
                       Ast.TyParam a;
                       Ast.TyApply (Ast.list_ty_name, [ Ast.TyParam a ]);
                     ]) );
            ] ));
    op_signatures = Ast.OpNameMap.empty;
  }

let print_tau_subst ?(tau_pp = PrettyPrint.TauPrintParam.create ()) p tau =
  Format.printf "TauConstraint(%t ↦ %t)"
    (PrettyPrint.print_tau (module Tau) tau_pp (Ast.TauParam p))
    (PrettyPrint.print_tau (module Tau) tau_pp tau)

let print_type_constraint ?(ty_pp = PrettyPrint.TyPrintParam.create ())
    ?(tau_pp = PrettyPrint.TauPrintParam.create ()) t1 t2 =
  Format.printf "TypeConstraint(%t = %t)"
    (PrettyPrint.print_ty (module Tau) ty_pp tau_pp t1)
    (PrettyPrint.print_ty (module Tau) ty_pp tau_pp t2)

let print_tau_constraint ?(tau_pp = PrettyPrint.TauPrintParam.create ()) tau1
    tau2 =
  Format.printf "TauConstraint(%t = %t)"
    (PrettyPrint.print_tau (module Tau) tau_pp tau1)
    (PrettyPrint.print_tau (module Tau) tau_pp tau2)

let print_tau_geq ?(tau_pp = PrettyPrint.TauPrintParam.create ()) tau1 tau2 =
  Format.printf "TauGeq(%t >= %t)"
    (PrettyPrint.print_tau (module Tau) tau_pp tau1)
    (PrettyPrint.print_tau (module Tau) tau_pp tau2)

let print_fresh_tau_constraint ?(tau_pp = PrettyPrint.TauPrintParam.create ())
    tau =
  Format.printf "FreshTauConstraint(%t)"
    (PrettyPrint.print_tau (module Tau) tau_pp tau)

let print_type_constraints ?(ty_pp = PrettyPrint.TyPrintParam.create ())
    ?(tau_pp = PrettyPrint.TauPrintParam.create ()) constraints =
  Format.fprintf Format.std_formatter "[@[<hov>%a@]]@."
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
       (fun _ppf constraint_ ->
         match constraint_ with
         | t1, t2 -> print_type_constraint t1 t2 ~ty_pp ~tau_pp))
    constraints

let print_tau_eq_constraints ?(tau_pp = PrettyPrint.TauPrintParam.create ())
    constraints =
  Format.fprintf Format.std_formatter "[@[<hov>%a@]]@."
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
       (fun _ppf constraint_ ->
         match constraint_ with
         | tau1, tau2 -> print_tau_constraint tau1 tau2 ~tau_pp))
    constraints

let print_tau_ineq_constraints ?(tau_pp = PrettyPrint.TauPrintParam.create ())
    constraints =
  Format.fprintf Format.std_formatter "[@[<hov>%a@]]@."
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
       (fun _ppf constraint_ ->
         match constraint_ with tau1, tau2 -> print_tau_geq tau1 tau2 ~tau_pp))
    constraints

let print_tau_abs_constraints ?(tau_pp = PrettyPrint.TauPrintParam.create ())
    constraints =
  Format.fprintf Format.std_formatter "[@[<hov>%a@]]@."
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
       (fun _ppf constraint_ ->
         match constraint_ with tau -> print_fresh_tau_constraint tau ~tau_pp))
    constraints

let rec check_ty state = function
  | Ast.TyConst _ -> ()
  | TyApply (ty_name, tys) ->
      let params, _ = Ast.TyNameMap.find ty_name state.type_definitions in
      let expected, actual = (List.length params, List.length tys) in
      if expected <> actual then
        Error.typing "Type %t expects %d arguments but got %d."
          (Ast.TyName.print ty_name) expected actual
      else List.iter (check_ty state) tys
  | TyParam _ -> ()
  | TyArrow (ty1, ty2) ->
      check_ty state ty1;
      check_comp_ty state ty2
  | TyTuple tys -> List.iter (check_ty state) tys
  | TyBox (_, ty) -> check_ty state ty
  | TyHandler (ty1, ty2) ->
      check_comp_ty state ty1;
      check_comp_ty state ty2

and check_comp_ty state = function Ast.CompTy (ty, _tau) -> check_ty state ty

let check_variant state (_label, arg_ty) =
  match arg_ty with None -> () | Some ty -> check_ty state ty

let check_ty_def state = function
  | Ast.TySum defs -> List.iter (check_variant state) defs
  | Ast.TyInline ty -> check_ty state ty

let fresh_ty () =
  let a = Ast.TyParamModule.fresh "ty" in
  Ast.TyParam a

let fresh_tau () =
  let t = Ast.TauParamModule.fresh "tau" in
  Ast.TauParam t

let fresh_comp_ty () = Ast.CompTy (fresh_ty (), fresh_tau ())

let extend_variables state vars =
  List.fold_left
    (fun state (x, ty) ->
      let updated_variables =
        ContextHolderModule.add_variable x ([], [], ty) state.variables
      in
      { state with variables = updated_variables })
    state vars

let extend_temporal state t =
  let updated_variables = ContextHolderModule.add_temp t state.variables in
  { state with variables = updated_variables }

let refreshing_ty_subst params =
  List.fold_left
    (fun subst param ->
      let ty = fresh_ty () in
      Ast.TyParamMap.add param ty subst)
    Ast.TyParamMap.empty params

let refreshing_tau_subst params =
  List.fold_left
    (fun subst param ->
      let tau = fresh_tau () in
      Ast.TauParamMap.add param tau subst)
    Ast.TauParamMap.empty params

let build_from_list f_nil f_cons params =
  match params with
  | [] -> f_nil
  | hd :: tl -> List.fold_left (fun acc e -> f_cons acc e) hd tl

let infer_variant state lbl =
  let rec find = function
    | [] -> assert false
    | (_, (_, Ast.TyInline _)) :: ty_defs -> find ty_defs
    | (ty_name, (params, Ast.TySum variants)) :: ty_defs -> (
        match List.assoc_opt lbl variants with
        | None -> find ty_defs
        | Some ty -> (ty_name, params, ty))
  in
  let ty_name, params, ty =
    find (Ast.TyNameMap.bindings state.type_definitions)
  in
  let ty_subst = refreshing_ty_subst params in
  let tau_subst = refreshing_tau_subst [] in
  let args = List.map (fun param -> Ast.TyParamMap.find param ty_subst) params
  and ty' = Option.map (Ast.substitute_ty ty_subst tau_subst) ty in
  (ty', Ast.TyApply (ty_name, args))

let rec infer_pattern state = function
  | Ast.PVar x ->
      let ty = fresh_ty () in
      (ty, [ (x, ty) ], [])
  | Ast.PAs (pat, x) ->
      let ty, vars, eqs = infer_pattern state pat in
      (ty, (x, ty) :: vars, eqs)
  | Ast.PAnnotated (pat, ty) ->
      let ty', vars, eqs = infer_pattern state pat in
      (ty, vars, (ty, ty') :: eqs)
  | Ast.PConst c -> (Ast.TyConst (Const.infer_ty c), [], [])
  | Ast.PNonbinding ->
      let ty = fresh_ty () in
      (ty, [], [])
  | Ast.PTuple pats ->
      let fold pat (tys, vars, eqs) =
        let ty', vars', eqs' = infer_pattern state pat in
        (ty' :: tys, vars' @ vars, eqs' @ eqs)
      in
      let tys, vars, eqs = List.fold_right fold pats ([], [], []) in
      (Ast.TyTuple tys, vars, eqs)
  | Ast.PVariant (lbl, pat) -> (
      let ty_in, ty_out = infer_variant state lbl in
      match (ty_in, pat) with
      | None, None -> (ty_out, [], [])
      | Some ty_in, Some pat ->
          let ty, vars, eqs = infer_pattern state pat in
          (ty_out, vars, (ty_in, ty) :: eqs)
      | None, Some _ | Some _, None ->
          Error.typing "Variant optional argument mismatch")

let rec infer_expression state = function
  | Ast.Var x ->
      let ty_params, tau_params, ty =
        ContextHolderModule.find_variable x state.variables
      in
      let ty_subst = refreshing_ty_subst ty_params in
      let tau_subst = refreshing_tau_subst tau_params in
      (Ast.substitute_ty ty_subst tau_subst ty, [], [], [], [])
      (* type, type constraints, tau constraints, tau inequational constraints, tau abstractness constraints *)
  | Ast.Const c -> (Ast.TyConst (Const.infer_ty c), [], [], [], [])
  | Ast.Annotated (expr, ty) ->
      let ty', ty_eqs, tau_eqs, tau_ineqs, tau_abs =
        infer_expression state expr
      in
      (ty, (ty, ty') :: ty_eqs, tau_eqs, tau_ineqs, tau_abs)
  | Ast.Tuple exprs ->
      let fold expr (tys, ty_eqs, tau_eqs, tau_ineqs, tau_abs) =
        let ty', ty_eqs', tau_eqs', tau_ineqs', tau_abs' =
          infer_expression state expr
        in
        ( ty' :: tys,
          ty_eqs' @ ty_eqs,
          tau_eqs' @ tau_eqs,
          tau_ineqs' @ tau_ineqs,
          tau_abs' @ tau_abs )
      in
      let tys, ty_eqs, tau_eqs, tau_ineqs, tau_abs =
        List.fold_right fold exprs ([], [], [], [], [])
      in
      (Ast.TyTuple tys, ty_eqs, tau_eqs, tau_ineqs, tau_abs)
  | Ast.Lambda abs ->
      let ty, ty', ty_eqs, tau_eqs, tau_ineqs, tau_abs =
        infer_abstraction state abs
      in
      (Ast.TyArrow (ty, ty'), ty_eqs, tau_eqs, tau_ineqs, tau_abs)
  | Ast.PureLambda abs ->
      let ty, Ast.CompTy (ty', tau), ty_eqs, tau_eqs, tau_ineqs, tau_abs =
        infer_abstraction state abs
      in
      ( Ast.TyArrow (ty, CompTy (ty', tau)),
        ty_eqs,
        (tau, Ast.TauConst Tau.zero) :: tau_eqs,
        tau_ineqs,
        tau_abs )
  | Ast.RecLambda (f, abs) ->
      let f_ty = fresh_ty () in
      let state' = extend_variables state [ (f, f_ty) ] in
      let ty, CompTy (ty', tau), ty_eqs, tau_eqs, tau_ineqs, tau_abs =
        infer_abstraction state' abs
      in
      let out_ty = Ast.TyArrow (ty, CompTy (ty', tau)) in
      ( out_ty,
        (f_ty, out_ty) :: ty_eqs,
        (tau, Ast.TauConst Tau.zero) :: tau_eqs,
        tau_ineqs,
        tau_abs )
  | Ast.Variant (lbl, expr) -> (
      let ty_in, ty_out = infer_variant state lbl in
      match (ty_in, expr) with
      | None, None -> (ty_out, [], [], [], [])
      | Some ty_in, Some expr ->
          let ty, ty_eqs, tau_eqs, tau_ineqs, tau_abs =
            infer_expression state expr
          in
          (ty_out, (ty_in, ty) :: ty_eqs, tau_eqs, tau_ineqs, tau_abs)
      | None, Some _ | Some _, None ->
          Error.typing "Variant optional argument mismatch")
  | Ast.Handler (ret_case, op_cases) ->
      let arg_tau = fresh_tau () in
      let state' = extend_temporal state arg_tau in
      let ( arg_ty,
            Ast.CompTy (ret_ty, ret_tau),
            ty_eqs,
            tau_eqs,
            tau_ineqs,
            tau_abs ) =
        infer_abstraction state' ret_case
      in
      let ty_eqs', tau_eqs', tau_ineqs', tau_abs' =
        Ast.OpNameMap.fold
          (fun op op_case (ty_eqs'', tau_eqs'', tau_ineqs'', tau_abs'') ->
            let op_sig = Ast.OpNameMap.find_opt op state.op_signatures in
            match op_sig with
            | None -> Error.typing "Case for an unknown operation."
            | Some (param_ty, arity_ty, op_tau) ->
                let ( op_args_ty,
                      Ast.CompTy (op_case_ty, op_case_tau),
                      op_ty_eqs,
                      op_tau_eqs,
                      op_tau_ineqs,
                      op_tau_abs ) =
                  infer_abstraction state op_case
                in
                let tau = fresh_tau () in
                let op_ty_eqs' =
                  (op_case_ty, ret_ty)
                  :: ( op_args_ty,
                       Ast.TyTuple
                         [
                           param_ty;
                           Ast.TyBox
                             ( op_tau,
                               Ast.TyArrow (arity_ty, CompTy (ret_ty, tau)) );
                         ] )
                  :: op_ty_eqs
                in
                let op_tau_eqs' =
                  (op_case_tau, Ast.TauAdd (op_tau, tau)) :: op_tau_eqs
                in
                let op_tau_ineqs' = op_tau_ineqs in
                let op_tau_abs' = tau :: op_tau_abs in
                ( op_ty_eqs' @ ty_eqs'',
                  op_tau_eqs' @ tau_eqs'',
                  op_tau_ineqs' @ tau_ineqs'',
                  op_tau_abs' @ tau_abs'' ))
          op_cases ([], [], [], [])
      in
      ( Ast.TyHandler (CompTy (arg_ty, arg_tau), CompTy (ret_ty, ret_tau)),
        ty_eqs @ ty_eqs',
        tau_eqs @ tau_eqs',
        tau_ineqs @ tau_ineqs',
        tau_abs @ tau_abs' )

and infer_computation state = function
  | Ast.Return expr ->
      let ty, ty_eqs, tau_eqs, tau_ineqs, tau_abs =
        infer_expression state expr
      in
      ( Ast.CompTy (ty, Ast.TauConst Tau.zero),
        ty_eqs,
        tau_eqs,
        tau_ineqs,
        tau_abs )
  | Ast.Do (comp1, comp2) ->
      let CompTy (ty1, tau1), ty_eqs1, tau_eqs1, tau_ineqs1, tau_abs1 =
        infer_computation state comp1
      in
      let comp_tau = fresh_tau () in
      let state' = extend_temporal state comp_tau in
      let ty1', Ast.CompTy (ty2, tau2), ty_eqs2, tau_eqs2, tau_ineqs2, tau_abs2
          =
        infer_abstraction state' comp2
      in
      ( CompTy (ty2, Ast.TauAdd (comp_tau, tau2)),
        ((ty1, ty1') :: ty_eqs1) @ ty_eqs2,
        ((tau1, comp_tau) :: tau_eqs1) @ tau_eqs2,
        tau_ineqs1 @ tau_ineqs2,
        tau_abs1 @ tau_abs2 )
  | Ast.Apply (e1, e2) ->
      let t1, ty_eqs1, tau_eqs1, tau_ineqs1, tau_abs1 =
        infer_expression state e1
      and t2, ty_eqs2, tau_eqs2, tau_ineqs2, tau_abs2 =
        infer_expression state e2
      and a = fresh_comp_ty () in
      ( a,
        ((t1, Ast.TyArrow (t2, a)) :: ty_eqs1) @ ty_eqs2,
        tau_eqs1 @ tau_eqs2,
        tau_ineqs1 @ tau_ineqs2,
        tau_abs1 @ tau_abs2 )
  | Ast.Match (e, cases) ->
      let ty1, ty_eqs, tau_eqs, tau_ineqs, tau_abs = infer_expression state e
      and branch_ty = fresh_ty () in
      let fold (taus, ty_eqs, tau_eqs, tau_ineqs, tau_abs) abs =
        let ( ty1',
              CompTy (branch_ty', branch_tau'),
              ty_eqs',
              tau_eqs',
              tau_ineqs',
              tau_abs' ) =
          infer_abstraction state abs
        in
        ( branch_tau' :: taus,
          ((ty1, ty1') :: (branch_ty, branch_ty') :: ty_eqs') @ ty_eqs,
          tau_eqs' @ tau_eqs,
          tau_ineqs' @ tau_ineqs,
          tau_abs' @ tau_abs )
      in
      let taus'', ty_eqs'', tau_eqs'', tau_ineqs'', tau_abs'' =
        List.fold_left fold ([], ty_eqs, tau_eqs, tau_ineqs, tau_abs) cases
      in
      ( CompTy
          ( branch_ty,
            build_from_list (Ast.TauConst Tau.zero)
              (fun acc e -> Ast.TauMeet (acc, e))
              taus'' ),
        ty_eqs'',
        tau_eqs'',
        tau_ineqs'',
        tau_abs'' )
  | Ast.Delay (tau, c) ->
      let state' = extend_temporal state tau in
      let CompTy (ty, tau'), ty_eqs, tau_eqs, tau_ineqs, tau_abs =
        infer_computation state' c
      in
      (CompTy (ty, Ast.TauAdd (tau, tau')), ty_eqs, tau_eqs, tau_ineqs, tau_abs)
  | Ast.Box (tau, e, abs) ->
      let state_ahead = extend_temporal state tau in
      let value_ty, ty_eqs, tau_eqs, tau_ineqs, tau_abs =
        infer_expression state_ahead e
      in
      let value_ty', comp_ty, ty_eqs', tau_eqs', tau_ineqs', tau_abs' =
        infer_abstraction state abs
      in
      ( comp_ty,
        ((Ast.TyBox (tau, value_ty), value_ty') :: ty_eqs) @ ty_eqs',
        tau_eqs @ tau_eqs',
        tau_ineqs @ tau_ineqs',
        tau_abs @ tau_abs' )
  | Ast.Unbox (e, abs) ->
      let rec findVar e =
        match e with
        | Ast.Var x -> x
        | Ast.Annotated (e', _) -> findVar e'
        | _ -> Error.typing "Unboxing requires a variable."
      in
      let var = findVar e in
      let sum_taus_added_after =
        ContextHolderModule.sum_taus_added_after var state.variables
      in
      let boxed_ty, ty_eqs, tau_eqs, tau_ineqs, tau_abs =
        infer_expression state e
      in
      let value_ty, comp_ty, ty_eqs', tau_eqs', tau_ineqs', tau_abs' =
        infer_abstraction state abs
      in
      let tau = fresh_tau () in
      ( comp_ty,
        ((Ast.TyBox (tau, value_ty), boxed_ty) :: ty_eqs) @ ty_eqs',
        tau_eqs @ tau_eqs',
        ((sum_taus_added_after, tau) :: tau_ineqs) @ tau_ineqs',
        tau_abs @ tau_abs' )
  | Ast.Perform (op, e, abs) -> (
      let op_sig = Ast.OpNameMap.find_opt op state.op_signatures in
      match op_sig with
      | None -> Error.typing "Unknown operation call."
      | Some (param_ty, arity_ty, op_tau) ->
          let value_ty, ty_eqs, tau_eqs, tau_ineqs, tau_abs =
            infer_expression state e
          in
          let state_ahead = extend_temporal state op_tau in
          let ( value_ty',
                CompTy (cont_ty, cont_tau),
                ty_eqs',
                tau_eqs',
                tau_ineqs',
                tau_abs' ) =
            infer_abstraction state_ahead abs
          in
          ( CompTy (cont_ty, Ast.TauAdd (op_tau, cont_tau)),
            ((value_ty, param_ty) :: (value_ty', arity_ty) :: ty_eqs) @ ty_eqs',
            tau_eqs @ tau_eqs',
            tau_ineqs @ tau_ineqs',
            tau_abs @ tau_abs' ))
  | Ast.Handle (c, h) ->
      let CompTy (ty, tau), ty_eqs, tau_eqs, tau_ineqs, tau_abs =
        infer_computation state c
      in
      let ty', ty_eqs', tau_eqs', tau_ineqs', tau_abs' =
        infer_expression state h
      in
      let ty'' = fresh_ty () in
      let tau'' = fresh_tau () in
      ( CompTy (ty'', Ast.TauAdd (tau, tau'')),
        ((ty', Ast.TyHandler (CompTy (ty, tau), CompTy (ty'', tau''))) :: ty_eqs)
        @ ty_eqs',
        tau_eqs @ tau_eqs',
        tau_ineqs @ tau_ineqs',
        tau_abs @ tau_abs' )

and infer_abstraction state (pat, comp) =
  let ty, vars, ty_eqs = infer_pattern state pat in
  let state' = extend_variables state vars in
  let ty', ty_eqs', tau_eqs', tau_ineqs', tau_abs' =
    infer_computation state' comp
  in
  (ty, ty', ty_eqs @ ty_eqs', tau_eqs', tau_ineqs', tau_abs')

let subst_ty_equations ty_subst tau_subst =
  let subst_ty_equation = function
    | t1, t2 ->
        ( Ast.substitute_ty ty_subst tau_subst t1,
          Ast.substitute_ty ty_subst tau_subst t2 )
  in
  List.map subst_ty_equation

let subst_tau_equations tau_subst =
  let subst_tau_equation = function
    | tau1, tau2 ->
        (Ast.substitute_tau tau_subst tau1, Ast.substitute_tau tau_subst tau2)
  in
  List.map subst_tau_equation

let subst_tau_inequations tau_subst =
  let subst_inequation = function
    | tau1, tau2 ->
        (Ast.substitute_tau tau_subst tau1, Ast.substitute_tau tau_subst tau2)
  in
  List.map subst_inequation

let subst_tau_abstract_constraints tau_subst =
  let subst_abstract_constraint = function
    | tau -> Ast.substitute_tau tau_subst tau
  in
  List.map subst_abstract_constraint

let add_ty_subst a ty ty_subst tau_subst =
  Ast.TyParamMap.add a (Ast.substitute_ty ty_subst tau_subst ty) ty_subst

let add_tau_subst tp tau tau_subst =
  Ast.TauParamMap.add tp (Ast.substitute_tau tau_subst tau) tau_subst

let rec occurs_ty a = function
  | Ast.TyParam a' -> a = a'
  | Ast.TyConst _ -> false
  | Ast.TyArrow (ty1, CompTy (ty2, _)) -> occurs_ty a ty1 || occurs_ty a ty2
  | Ast.TyApply (_, tys) -> List.exists (occurs_ty a) tys
  | Ast.TyTuple tys -> List.exists (occurs_ty a) tys
  | Ast.TyBox (_, ty) -> occurs_ty a ty
  | Ast.TyHandler (CompTy (ty1, _), CompTy (ty2, _)) ->
      occurs_ty a ty1 || occurs_ty a ty2

let rec occurs_tau a = function
  | Ast.TauParam a' -> a = a'
  | Ast.TauConst _ -> false
  | Ast.TauAdd (tau, tau') -> occurs_tau a tau || occurs_tau a tau'
  | Ast.TauMeet (tau, tau') -> occurs_tau a tau || occurs_tau a tau'

let is_transparent_type state ty_name =
  match Ast.TyNameMap.find ty_name state.type_definitions with
  | _, Ast.TySum _ -> false
  | _, Ast.TyInline _ -> true

let unfold state ty_name args =
  match Ast.TyNameMap.find ty_name state.type_definitions with
  | _, Ast.TySum _ -> assert false
  | params, Ast.TyInline ty ->
      let ty_subst =
        List.combine params args |> List.to_seq |> Ast.TyParamMap.of_seq
      in
      let tau_subst = refreshing_tau_subst [] in
      Ast.substitute_ty ty_subst tau_subst ty

let compare_tau a b =
  match (a, b) with
  | Either.Left p1, Either.Left p2 -> compare p1 p2
  | Either.Right c1, Either.Right c2 -> compare c1 c2
  | Either.Left _, _ -> -1
  | _, Either.Left _ -> 1

let build_tau_param_list tau =
  let rec aux acc tau =
    match tau with
    | Ast.TauParam t -> Either.Left t :: acc
    | Ast.TauConst c -> Either.Right c :: acc
    | Ast.TauAdd (tau1, tau2) ->
        let acc' = aux acc tau2 in
        aux acc' tau1
    | Ast.TauMeet (_tau1, _tau2) ->
        let tau_pp = PrettyPrint.TauPrintParam.create () in
        Error.typing "Expected a meet-free temporal grade %t"
          (PrettyPrint.print_tau (module Tau) tau_pp tau)
  in
  aux [] tau

let rec sum_sorted_tau_param_list_constants = function
  | [] -> []
  | Either.Left t :: tl ->
      Either.Left t :: sum_sorted_tau_param_list_constants tl
  | Either.Right c :: Either.Right c' :: tl ->
      sum_sorted_tau_param_list_constants (Either.Right (Tau.add c c') :: tl)
  | Either.Right c :: tl ->
      Either.Right c :: sum_sorted_tau_param_list_constants tl

let build_sorted_summed_tau_param_list tau =
  build_tau_param_list tau |> List.sort compare_tau
  |> sum_sorted_tau_param_list_constants

let build_tau_param_lists tau =
  let rec aux accs tau =
    match tau with
    | Ast.TauParam t -> List.map (fun acc -> Either.Left t :: acc) accs
    | Ast.TauConst c -> List.map (fun acc -> Either.Right c :: acc) accs
    | Ast.TauAdd (tau1, tau2) ->
        let accs' = aux accs tau2 in
        aux accs' tau1
    | Ast.TauMeet (tau1, tau2) -> aux accs tau1 @ aux accs tau2
  in
  aux [ [] ] tau

let build_sorted_summed_tau_param_lists tau =
  build_tau_param_lists tau
  |> List.map (fun tau' -> List.sort compare_tau tau')
  |> List.map (fun tau' -> sum_sorted_tau_param_list_constants tau')

let cancel_common_elements left right =
  let rec aux l r acc_left acc_right =
    match (l, r) with
    | lhd :: ltl, rhd :: rtl ->
        if lhd = rhd then aux ltl rtl acc_left acc_right
        else if lhd < rhd then aux ltl r (lhd :: acc_left) acc_right
        else aux l rtl acc_left (rhd :: acc_right)
    | [], [] -> (acc_left, acc_right)
    | [], r -> (acc_left, acc_right @ r)
    | l, [] -> (acc_left @ l, acc_right)
  in
  aux left right [] []

let build_tau_from_param_list params =
  let to_tau = function
    | Either.Left x -> Ast.TauParam x
    | Either.Right x -> Ast.TauConst x
  in
  match params with
  | [] -> Ast.TauConst Tau.zero
  | hd :: tl ->
      List.fold_left (fun acc e -> Ast.TauAdd (acc, to_tau e)) (to_tau hd) tl

let build_tau_from_param_lists = function
  | [] -> Ast.TauConst Tau.zero
  | tau :: taus ->
      List.fold_left
        (fun acc x -> Ast.TauMeet (acc, build_tau_from_param_list x))
        (build_tau_from_param_list tau)
        taus

(* TODO: Currently some duplications between simplification and normalisation. Should be both folded into the latter. *)
let rec simplify_tau tau =
  match tau with
  | Ast.TauAdd (t1, t2) -> (
      let t1' = simplify_tau t1 in
      let t2' = simplify_tau t2 in
      match (t1', t2') with
      | Ast.TauConst c1, Ast.TauConst c2 -> Ast.TauConst (Tau.add c1 c2)
      | Ast.TauConst z, t when z = Tau.zero -> t
      | t, Ast.TauConst z when z = Tau.zero -> t
      | _ -> Ast.TauAdd (t1', t2'))
  | Ast.TauMeet (t1, t2) -> (
      let t1' = simplify_tau t1 in
      let t2' = simplify_tau t2 in
      match (t1', t2') with
      | _, _ when t1' = t2' -> t1'
      | Ast.TauConst z, _ when z = Tau.zero -> Ast.TauConst z
      | _, Ast.TauConst z when z = Tau.zero -> Ast.TauConst z
      | Ast.TauConst t1, Ast.TauConst t2 -> Ast.TauConst (Tau.meet t1 t2)
      | _ -> Ast.TauMeet (t1', t2'))
  | _ -> tau

and simplify_ty ty =
  match ty with
  | Ast.TyConst t -> Ast.TyConst t
  | TyApply (ty_name, ty_list) -> TyApply (ty_name, List.map simplify_ty ty_list)
  | TyParam ty_param -> TyParam ty_param
  | TyArrow (ty, Ast.CompTy (ty', tau')) ->
      TyArrow (simplify_ty ty, Ast.CompTy (simplify_ty ty', simplify_tau tau'))
  | TyTuple ty_list -> TyTuple (List.map simplify_ty ty_list)
  | TyBox (tau, ty) -> TyBox (simplify_tau tau, simplify_ty ty)
  | TyHandler (Ast.CompTy (ty1, tau1), Ast.CompTy (ty2, tau2)) ->
      TyHandler
        ( Ast.CompTy (simplify_ty ty1, simplify_tau tau1),
          Ast.CompTy (simplify_ty ty2, simplify_tau tau2) )

let simplify_comp_ty = function
  | Ast.CompTy (ty, tau) -> Ast.CompTy (simplify_ty ty, simplify_tau tau)

(* TODO: Currently some duplications between simplification and normalisation. Should be both folded into the latter. *)
let rec normalise_tau tau =
  build_sorted_summed_tau_param_lists tau
  |> List.sort_uniq (List.compare compare_tau)
  |> build_tau_from_param_lists

and normalise_ty ty =
  match ty with
  | Ast.TyConst t -> Ast.TyConst t
  | TyApply (ty_name, ty_list) ->
      TyApply (ty_name, List.map normalise_ty ty_list)
  | TyParam ty_param -> TyParam ty_param
  | TyArrow (ty, Ast.CompTy (ty', tau')) ->
      TyArrow
        (normalise_ty ty, Ast.CompTy (normalise_ty ty', normalise_tau tau'))
  | TyTuple ty_list -> TyTuple (List.map normalise_ty ty_list)
  | TyBox (tau, ty) -> TyBox (normalise_tau tau, normalise_ty ty)
  | TyHandler (Ast.CompTy (ty1, tau1), Ast.CompTy (ty2, tau2)) ->
      TyHandler
        ( Ast.CompTy (normalise_ty ty1, normalise_tau tau1),
          Ast.CompTy (normalise_ty ty2, normalise_tau tau2) )

let normalise_comp_ty = function
  | Ast.CompTy (ty, tau) -> Ast.CompTy (normalise_ty ty, normalise_tau tau)

let rec unify_ty_constraints state tau_eqs = function
  | [] -> (Ast.TyParamMap.empty, tau_eqs)
  | (t1, t2) :: ty_eqs when t1 = t2 -> unify_ty_constraints state tau_eqs ty_eqs
  | (Ast.TyApply (ty_name1, args1), Ast.TyApply (ty_name2, args2)) :: ty_eqs
    when ty_name1 = ty_name2 ->
      let new_eqs =
        List.map (fun (t1, t2) -> (t1, t2)) (List.combine args1 args2)
      in
      unify_ty_constraints state tau_eqs (new_eqs @ ty_eqs)
  | (Ast.TyApply (ty_name, args), ty) :: ty_eqs
    when is_transparent_type state ty_name ->
      unify_ty_constraints state tau_eqs
        ((unfold state ty_name args, ty) :: ty_eqs)
  | (ty, Ast.TyApply (ty_name, args)) :: ty_eqs
    when is_transparent_type state ty_name ->
      unify_ty_constraints state tau_eqs
        ((ty, unfold state ty_name args) :: ty_eqs)
  | (Ast.TyTuple tys1, Ast.TyTuple tys2) :: ty_eqs
    when List.length tys1 = List.length tys2 ->
      let new_eqs =
        List.map (fun (t1, t2) -> (t1, t2)) (List.combine tys1 tys2)
      in
      unify_ty_constraints state tau_eqs (new_eqs @ ty_eqs)
  | ( Ast.TyArrow (t1, CompTy (t1', tau1')),
      Ast.TyArrow (t2, CompTy (t2', tau2')) )
    :: ty_eqs ->
      unify_ty_constraints state
        ((tau1', tau2') :: tau_eqs)
        ((t1, t2) :: (t1', t2') :: ty_eqs)
  | (Ast.TyParam a, t) :: ty_eqs when not (occurs_ty a t) ->
      let ty_subst, tau_eqs' =
        unify_ty_constraints state tau_eqs
          (subst_ty_equations
             (Ast.TyParamMap.singleton a t)
             Ast.TauParamMap.empty ty_eqs)
      in
      (add_ty_subst a t ty_subst Ast.TauParamMap.empty, tau_eqs')
  | (t, Ast.TyParam a) :: ty_eqs when not (occurs_ty a t) ->
      let ty_subst, tau_eqs' =
        unify_ty_constraints state tau_eqs
          (subst_ty_equations
             (Ast.TyParamMap.singleton a t)
             Ast.TauParamMap.empty ty_eqs)
      in
      (add_ty_subst a t ty_subst Ast.TauParamMap.empty, tau_eqs')
  | (Ast.TyBox (tau1, ty1), Ast.TyBox (tau2, ty2)) :: ty_eqs ->
      unify_ty_constraints state ((tau1, tau2) :: tau_eqs) ((ty1, ty2) :: ty_eqs)
  | ( Ast.TyHandler (Ast.CompTy (ty1, tau1), Ast.CompTy (ty2, tau2)),
      Ast.TyHandler (Ast.CompTy (ty1', tau1'), Ast.CompTy (ty2', tau2')) )
    :: ty_eqs ->
      unify_ty_constraints state
        ((tau1, tau1') :: (tau2, tau2') :: tau_eqs)
        ((ty1, ty1') :: (ty2, ty2') :: ty_eqs)
  | (t1, t2) :: _ ->
      let ty_pp = PrettyPrint.TyPrintParam.create () in
      let tau_pp = PrettyPrint.TauPrintParam.create () in
      Error.typing "Cannot unify types %t = %t"
        (PrettyPrint.print_ty (module Tau) ty_pp tau_pp t1)
        (PrettyPrint.print_ty (module Tau) ty_pp tau_pp t2)

let rec unify_tau_constraints state prev_unsolved_size unsolved = function
  | [] ->
      let current_unsolved_size = List.length unsolved in
      if current_unsolved_size = 0 then
        (* All constraints solved *)
        Ast.TauParamMap.empty
      else if current_unsolved_size = prev_unsolved_size then
        Error.typing
          "Unification stuck - could not solve remaining constraints %t"
          (fun ppf ->
            print_tau_eq_constraints unsolved;
            Format.fprintf ppf "%s" "")
      else
        (* Retry with deferred constraints *)
        unify_tau_constraints state current_unsolved_size [] unsolved
  | (tau1, tau2) :: eqs -> (
      let tau1' = simplify_tau tau1 in
      let tau2' = simplify_tau tau2 in
      match (tau1', tau2') with
      | _ when tau1' = tau2' ->
          unify_tau_constraints state prev_unsolved_size unsolved eqs
      | Ast.TauParam tp, tau when not (occurs_tau tp tau) ->
          let tau_subst =
            unify_tau_constraints state prev_unsolved_size
              (subst_tau_equations (Ast.TauParamMap.singleton tp tau) unsolved)
              (subst_tau_equations (Ast.TauParamMap.singleton tp tau) eqs)
          in
          add_tau_subst tp tau tau_subst
      | tau, Ast.TauParam tp when not (occurs_tau tp tau) ->
          let tau_subst =
            unify_tau_constraints state prev_unsolved_size
              (subst_tau_equations (Ast.TauParamMap.singleton tp tau) unsolved)
              (subst_tau_equations (Ast.TauParamMap.singleton tp tau) eqs)
          in
          add_tau_subst tp tau tau_subst
      | Ast.TauConst z, Ast.TauAdd (t1, t2)
      | Ast.TauAdd (t1, t2), Ast.TauConst z
        when z = Tau.zero ->
          unify_tau_constraints state prev_unsolved_size unsolved
            ((t1, Ast.TauConst Tau.zero) :: (t2, Ast.TauConst Tau.zero) :: eqs)
      | t, Ast.TauAdd (t1, t2) | Ast.TauAdd (t1, t2), t ->
          let left = build_sorted_summed_tau_param_list t in
          let right =
            build_sorted_summed_tau_param_list (Ast.TauAdd (t1, t2))
          in
          let left', right' = cancel_common_elements left right in
          let left_tau = build_tau_from_param_list left' in
          let right_tau = build_tau_from_param_list right' in
          if left_tau = t && right_tau = Ast.TauAdd (t1, t2) then
            unify_tau_constraints state prev_unsolved_size
              ((left_tau, right_tau) :: unsolved)
              eqs
          else
            unify_tau_constraints state prev_unsolved_size unsolved
              ((left_tau, right_tau) :: eqs)
      | u1, u2 ->
          unify_tau_constraints state prev_unsolved_size ((u1, u2) :: unsolved)
            eqs)

let rec split_tau_ineq_constraints = function
  | [] -> []
  | (tau_greater_or_equal, tau_smaller) :: tau_ineqs ->
      let taus = build_sorted_summed_tau_param_lists tau_greater_or_equal in
      let tau_ineqs' =
        List.map (fun tau -> (build_tau_from_param_list tau, tau_smaller)) taus
      in
      tau_ineqs' @ split_tau_ineq_constraints tau_ineqs

let rec check_tau_ineq_constraints state = function
  | [] -> ()
  | (tau_greater_or_equal, tau_smaller) :: tau_ineqs -> (
      let tau_greater_or_equal_simplified = simplify_tau tau_greater_or_equal
      and tau_smaller_simplified = simplify_tau tau_smaller in

      let tau_greater_or_equal_val =
        let tau_list =
          build_sorted_summed_tau_param_list tau_greater_or_equal_simplified
        in
        if List.length tau_list = 0 then Tau.zero
        else
          match List.nth tau_list (List.length tau_list - 1) with
          | Either.Left _ -> Tau.zero
          | Either.Right c -> c
      in

      let maybe_tau_smaller =
        try Some (ContextHolderModule.eval_tau tau_smaller_simplified)
        with _exn -> None
      in

      match maybe_tau_smaller with
      | None ->
          Error.typing "Cannot compare temporal values %t >= %t"
            (fun ppf ->
              PrettyPrint.print_tau
                (module Tau)
                (PrettyPrint.TauPrintParam.create ())
                tau_greater_or_equal_simplified ppf)
            (fun ppf ->
              PrettyPrint.print_tau
                (module Tau)
                (PrettyPrint.TauPrintParam.create ())
                tau_smaller_simplified ppf)
      | Some tau_smaller_val ->
          if tau_smaller_val > tau_greater_or_equal_val then
            Error.typing "Cannot unify temporal values %t >= %t"
              (fun ppf ->
                PrettyPrint.print_tau
                  (module Tau)
                  (PrettyPrint.TauPrintParam.create ())
                  tau_greater_or_equal_simplified ppf)
              (fun ppf ->
                PrettyPrint.print_tau
                  (module Tau)
                  (PrettyPrint.TauPrintParam.create ())
                  tau_smaller_simplified ppf)
          else check_tau_ineq_constraints state tau_ineqs)

let rec check_tau_abs_constraints = function
  | [] -> ()
  | tau :: tau_abs -> (
      match tau with
      | Ast.TauParam _ -> check_tau_abs_constraints tau_abs
      | _ ->
          Error.typing "Temporal value is not in an abstract form %t"
            (fun ppf ->
              PrettyPrint.print_tau
                (module Tau)
                (PrettyPrint.TauPrintParam.create ())
                tau ppf))

let unify state ty_eqs tau_eqs tau_ineqs tau_abs =
  (* let _ty_pp = PrettyPrint.TyPrintParam.create () in
  let _tau_pp = PrettyPrint.TauPrintParam.create () in *)
  let ty_subst, tau_eqs' = unify_ty_constraints state [] ty_eqs in
  let tau_subst = unify_tau_constraints state 0 [] (tau_eqs @ tau_eqs') in
  let ty_subst' =
    Ast.TyParamMap.map
      (fun ty -> Ast.substitute_ty ty_subst tau_subst ty)
      ty_subst
  in
  let tau_ineqs' =
    split_tau_ineq_constraints (subst_tau_inequations tau_subst tau_ineqs)
  in
  check_tau_ineq_constraints state tau_ineqs';
  check_tau_abs_constraints (subst_tau_abstract_constraints tau_subst tau_abs);
  (ty_subst', tau_subst)

let infer state e =
  let comp_ty, ty_eqs, tau_eqs, tau_ineqs, tau_abs =
    infer_computation state e
  in
  let ty_subst, tau_subst = unify state ty_eqs tau_eqs tau_ineqs tau_abs in
  let comp_ty' = Ast.substitute_comp_ty ty_subst tau_subst comp_ty in
  normalise_comp_ty comp_ty'

let add_external_function x ty_sch state =
  {
    state with
    variables = ContextHolderModule.add_variable x ty_sch state.variables;
  }

let add_top_definition state x expr =
  let ty, ty_eqs, tau_eqs, tau_ineqs, tau_abs = infer_expression state expr in
  let ty_subst, tau_subst = unify state ty_eqs tau_eqs tau_ineqs tau_abs in
  let ty' = Ast.substitute_ty ty_subst tau_subst ty in
  let ty'' = normalise_ty ty' in
  let free_vars, free_taus = Ast.free_vars ty'' in
  let ty_sch =
    ( free_vars |> Ast.TyParamSet.elements,
      free_taus |> Ast.TauParamSet.elements,
      ty'' )
  in
  add_external_function x ty_sch state

let add_type_definitions state ty_defs =
  let state' =
    List.fold_left
      (fun state (params, ty_name, ty_def) ->
        {
          state with
          type_definitions =
            Ast.TyNameMap.add ty_name (params, ty_def) state.type_definitions;
        })
      state ty_defs
  in
  List.iter (fun (_, _, ty_def) -> check_ty_def state' ty_def) ty_defs;
  state'

let add_operation_signature state (op, ty1, ty2, tau) =
  let state' =
    {
      state with
      op_signatures = Ast.OpNameMap.add op (ty1, ty2, tau) state.op_signatures;
    }
  in
  state'

let load_primitive state x prim =
  let ty_sch = Primitives.primitive_type_scheme prim in
  add_external_function x ty_sch state
