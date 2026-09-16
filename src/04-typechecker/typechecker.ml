module Error = Utils.Error
module StringMap = Utils.StringMap
module Ast = Language.Ast
module Const = Language.Const
module Context = Language.Context
module Exception = Language.Exception
module PrettyPrint = Language.PrettyPrint

module Make (ResourceGrade : Language.ResourceGrade.Grade) = struct
  module ContextHolderModule =
    Context.Make (Ast.Variable) (Map.Make (Ast.Variable)) (ResourceGrade)

  module P = Primitives.Make (ResourceGrade)

  type var_type = Global | Local

  type state = {
    variables :
      (Ast.ty_param list
      * Ast.rho_param list
      * ResourceGrade.t Ast.constr list
      * ResourceGrade.t Ast.ty
      * var_type)
      ContextHolderModule.t;
        (** Each variable's generalised type scheme: the quantified type and
            grade parameters, the constraints qualifying them, and the type.
            Local variables are monomorphic and unqualified. *)
    type_definitions :
      (Ast.ty_param list * ResourceGrade.t Ast.ty_def) Ast.TyNameMap.t;
    noneternal_types : Ast.TyNameSet.t;
        (** The type names declared with [noneternal type ...]. Their values are
            never eternal, whatever their structure says, and so is anything
            built out of them. *)
    op_signatures :
      (ResourceGrade.t Ast.ty
      * ResourceGrade.t Ast.ty
      * ResourceGrade.t Ast.rho)
      Ast.OpNameMap.t;
    op_bounds : (int * int) StringMap.t;
        (** The declared runtime bounds [within (lo, hi)] of the operations, the
            cost model the timed-trace orders read. Keyed by the operation's
            surface name, which is the name that appears inside trace literals;
            the desugarer has already rejected duplicate operation names. *)
    op_defaults : Ast.OpNameSet.t;
        (** The operations that have been given a default implementation, kept
            so that a second one can be rejected. *)
  }

  let initial_state =
    {
      variables = ContextHolderModule.empty;
      noneternal_types = Ast.TyNameSet.empty;
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
      op_bounds = StringMap.empty;
      op_defaults = Ast.OpNameSet.empty;
    }

  let print_type_constraint t1 t2 ty_pp rho_pp =
    Format.printf "TypeConstraint(%t = %t)"
      (PrettyPrint.print_ty (module ResourceGrade) ty_pp rho_pp t1)
      (PrettyPrint.print_ty (module ResourceGrade) ty_pp rho_pp t2)

  let print_one_type_constraint t1 t2 =
    let ty_pp = PrettyPrint.TyPrintParam.create () in
    let rho_pp = PrettyPrint.RhoPrintParam.create () in
    print_type_constraint t1 t2 ty_pp rho_pp

  let print_rho_constraint rho1 rho2 rho_pp =
    Format.printf "RhoConstraint(%t = %t)"
      (PrettyPrint.print_rho (module ResourceGrade) rho_pp rho1)
      (PrettyPrint.print_rho (module ResourceGrade) rho_pp rho2)

  let print_one_rho_constraint rho1 rho2 =
    let rho_pp = PrettyPrint.RhoPrintParam.create () in
    print_rho_constraint rho1 rho2 rho_pp

  let print_rho_geq rho1 rho2 rho_pp =
    Format.printf "RhoGeq(%t %s %t)"
      (PrettyPrint.print_rho (module ResourceGrade) rho_pp rho1)
      ResourceGrade.is_sub_rho_symbol
      (PrettyPrint.print_rho (module ResourceGrade) rho_pp rho2)

  let print_one_rho_geq rho1 rho2 =
    let rho_pp = PrettyPrint.RhoPrintParam.create () in
    print_rho_geq rho1 rho2 rho_pp

  let print_ty_constraints_pp ty_pp rho_pp constraints =
    Format.fprintf Format.std_formatter "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
         (fun _ppf constraint_ ->
           match constraint_ with
           | t1, t2 -> print_type_constraint t1 t2 ty_pp rho_pp))
      constraints

  let print_ty_constraints constraints =
    let ty_pp = PrettyPrint.TyPrintParam.create () in
    let rho_pp = PrettyPrint.RhoPrintParam.create () in
    print_ty_constraints_pp ty_pp rho_pp constraints

  let print_rho_eq_constraints_pp rho_pp constraints =
    Format.fprintf Format.std_formatter "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
         (fun _ppf constraint_ ->
           match constraint_ with
           | rho1, rho2 -> print_rho_constraint rho1 rho2 rho_pp))
      constraints

  let print_rho_eq_constraints constraints =
    let rho_pp = PrettyPrint.RhoPrintParam.create () in
    print_rho_eq_constraints_pp rho_pp constraints

  let print_rho_ineq_constraints_pp rho_pp constraints =
    let ty_pp = PrettyPrint.TyPrintParam.create () in
    Format.fprintf Format.std_formatter "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
         (fun ppf constraint_ ->
           PrettyPrint.print_constr
             (module ResourceGrade)
             ty_pp rho_pp constraint_ ppf))
      constraints

  let print_rho_ineq_constraints constraints =
    let rho_pp = PrettyPrint.RhoPrintParam.create () in
    print_rho_ineq_constraints_pp rho_pp constraints

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

  and check_comp_ty state = function
    | Ast.CompTy (ty, _rho) -> check_ty state ty

  let check_variant state (_label, arg_ty) =
    match arg_ty with None -> () | Some ty -> check_ty state ty

  let check_ty_def state = function
    | Ast.TySum defs -> List.iter (check_variant state) defs
    | Ast.TyInline ty -> check_ty state ty

  let fresh_ty () =
    let a = Ast.TyParamModule.fresh "ty" in
    Ast.TyParam a

  let fresh_rho () =
    let t = Ast.RhoParamModule.fresh "rho" in
    Ast.RhoParam t

  let fresh_comp_ty () = Ast.CompTy (fresh_ty (), fresh_rho ())

  let extend_local_variables state vars =
    List.fold_left
      (fun state (x, ty) ->
        let updated_variables =
          ContextHolderModule.add_variable x ([], [], [], ty, Local)
            state.variables
        in
        { state with variables = updated_variables })
      state vars

  let extend_global_variables state vars =
    List.fold_left
      (fun state (x, ty) ->
        let updated_variables =
          ContextHolderModule.add_variable x ([], [], [], ty, Global)
            state.variables
        in
        { state with variables = updated_variables })
      state vars

  let extend_resource_grade state t =
    let updated_variables = ContextHolderModule.add_temp t state.variables in
    { state with variables = updated_variables }

  let refreshing_ty_subst params =
    List.fold_left
      (fun subst param ->
        let ty = fresh_ty () in
        Ast.TyParamMap.add param ty subst)
      Ast.TyParamMap.empty params

  let refreshing_rho_subst params =
    List.fold_left
      (fun subst param ->
        let rho = fresh_rho () in
        Ast.RhoParamMap.add param rho subst)
      Ast.RhoParamMap.empty params

  let instantiate_constrs ty_subst rho_subst origin constrs =
    List.map
      (fun c ->
        Ast.substitute_constr ty_subst rho_subst (Ast.with_origin origin c))
      constrs

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
    let rho_subst = refreshing_rho_subst [] in
    let args = List.map (fun param -> Ast.TyParamMap.find param ty_subst) params
    and ty' = Option.map (Ast.substitute_ty ty_subst rho_subst) ty in
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

  (** Returns:
      + inferred value type
      + equational constraints between types
      + equational constraints between rhos
      + inequational and eternality constraints *)
  let rec infer_expression state = function
    | Ast.Var x ->
        let ty_params, rho_params, constrs, ty, var_type =
          ContextHolderModule.find_variable x state.variables
        in
        let rho_ineq =
          match var_type with
          | Local ->
              [
                Ast.EternalOrIneq
                  ( ty,
                    ContextHolderModule.sum_rhos_added_after x state.variables,
                    Ast.RhoConst ResourceGrade.zero,
                    Ast.UseOf x );
              ]
          | Global -> []
        in
        (* Format.fprintf Format.std_formatter "\n";
        PrettyPrint.print_expression
          (module ResourceGrade)
          (Ast.Var x) Format.std_formatter;
        PrettyPrint.print_rho
          (module ResourceGrade)
          (PrettyPrint.RhoPrintParam.create ())
          sum_rhos_added_after Format.std_formatter;
        Format.fprintf Format.std_formatter "\n"; *)
        let ty_subst = refreshing_ty_subst ty_params in
        let rho_subst = refreshing_rho_subst rho_params in
        (* The qualifier of the scheme is instantiated along with the type,
           and its constraints are then owed by this use of [x]. *)
        let constrs' =
          instantiate_constrs ty_subst rho_subst (Ast.InstanceOf x) constrs
        in
        (Ast.substitute_ty ty_subst rho_subst ty, [], [], rho_ineq @ constrs')
    | Ast.Const c -> (Ast.TyConst (Const.infer_ty c), [], [], [])
    | Ast.Annotated (expr, ty) -> (
        let ty', ty_eqs, rho_eqs, rho_ineqs = infer_expression state expr in
        match (ty, ty') with
        | ( Ast.TyArrow (arg_ty, CompTy (res_ty, rho)),
            Ast.TyArrow (arg_ty', CompTy (res_ty', rho')) ) ->
            (* An annotation on a function is a sub-effecting coercion, as an
               operation case or a default implementation is: the grade the
               body accumulates need only be a sub-grade of the stated one,
               while the argument and result types must agree exactly. *)
            ( ty,
              (arg_ty, arg_ty') :: (res_ty, res_ty') :: ty_eqs,
              rho_eqs,
              Ast.Ineq (rho', rho) :: rho_ineqs )
        | _ -> (ty, (ty, ty') :: ty_eqs, rho_eqs, rho_ineqs))
    | Ast.Tuple exprs ->
        let fold expr (tys, ty_eqs, rho_eqs, rho_ineqs) =
          let ty', ty_eqs', rho_eqs', rho_ineqs' =
            infer_expression state expr
          in
          ( ty' :: tys,
            ty_eqs' @ ty_eqs,
            rho_eqs' @ rho_eqs,
            rho_ineqs' @ rho_ineqs )
        in
        let tys, ty_eqs, rho_eqs, rho_ineqs =
          List.fold_right fold exprs ([], [], [], [])
        in
        (Ast.TyTuple tys, ty_eqs, rho_eqs, rho_ineqs)
    | Ast.Lambda abs ->
        let ty, ty', ty_eqs, rho_eqs, rho_ineqs = infer_abstraction state abs in
        (Ast.TyArrow (ty, ty'), ty_eqs, rho_eqs, rho_ineqs)
    | Ast.PureLambda abs ->
        let ty, Ast.CompTy (ty', rho), ty_eqs, rho_eqs, rho_ineqs =
          infer_abstraction state abs
        in
        ( Ast.TyArrow (ty, CompTy (ty', rho)),
          ty_eqs,
          (rho, Ast.RhoConst ResourceGrade.zero) :: rho_eqs,
          rho_ineqs )
    | Ast.RecLambda (f, abs) ->
        let f_ty = fresh_ty () in
        let state' = extend_local_variables state [ (f, f_ty) ] in
        let ty, CompTy (ty', rho), ty_eqs, rho_eqs, rho_ineqs =
          infer_abstraction state' abs
        in
        let out_ty = Ast.TyArrow (ty, CompTy (ty', rho)) in
        ( out_ty,
          (f_ty, out_ty) :: ty_eqs,
          (rho, Ast.RhoConst ResourceGrade.zero) :: rho_eqs,
          rho_ineqs )
    | Ast.Variant (lbl, expr) -> (
        let ty_in, ty_out = infer_variant state lbl in
        match (ty_in, expr) with
        | None, None -> (ty_out, [], [], [])
        | Some ty_in, Some expr ->
            let ty, ty_eqs, rho_eqs, rho_ineqs = infer_expression state expr in
            (ty_out, (ty_in, ty) :: ty_eqs, rho_eqs, rho_ineqs)
        | None, Some _ | Some _, None ->
            Error.typing "Variant optional argument mismatch")
    | Ast.Handler (ret_case, op_cases) ->
        let arg_rho = fresh_rho () in
        let state' = extend_resource_grade state arg_rho in
        let arg_ty, Ast.CompTy (ret_ty, ret_rho), ty_eqs, rho_eqs, rho_ineqs =
          infer_abstraction state' ret_case
        in
        let ty_eqs', rho_eqs', rho_ineqs' =
          Ast.OpNameMap.fold
            (fun op op_case (ty_eqs'', rho_eqs'', rho_ineqs'') ->
              let op_sig = Ast.OpNameMap.find_opt op state.op_signatures in
              match op_sig with
              | None -> Error.typing "Case for an unknown operation."
              | Some (param_ty, arity_ty, op_rho) ->
                  let ( op_args_ty,
                        Ast.CompTy (op_case_ty, op_case_rho),
                        op_ty_eqs,
                        op_rho_eqs,
                        op_rho_ineqs ) =
                    infer_abstraction state op_case
                  in
                  (* The case must be well-typed for every grade of the
                     continuation, so that grade is rigid. *)
                  let rho = Ast.RhoRigid (Ast.RhoParamModule.fresh "rho") in
                  let op_ty_eqs' =
                    (op_case_ty, ret_ty)
                    :: ( op_args_ty,
                         Ast.TyTuple
                           [
                             param_ty;
                             Ast.TyBox
                               ( op_rho,
                                 Ast.TyArrow (arity_ty, CompTy (ret_ty, rho)) );
                           ] )
                    :: op_ty_eqs
                  in
                  let op_rho_eqs' = op_rho_eqs in
                  (* The clause need only be a sub-effect of the operation's
                     declared grade extended by the continuation's grade
                     [rho] (sub-effecting), so that e.g. a clause performing
                     [Send] once realises the grade [{Send | Send; Send}]. *)
                  let op_rho_ineqs' =
                    Ast.Ineq (op_case_rho, Ast.RhoAdd (op_rho, rho))
                    :: op_rho_ineqs
                  in
                  ( op_ty_eqs' @ ty_eqs'',
                    op_rho_eqs' @ rho_eqs'',
                    op_rho_ineqs' @ rho_ineqs'' ))
            op_cases ([], [], [])
        in
        ( Ast.TyHandler (CompTy (arg_ty, arg_rho), CompTy (ret_ty, ret_rho)),
          ty_eqs @ ty_eqs',
          rho_eqs @ rho_eqs',
          rho_ineqs @ rho_ineqs' )

  (** Returns:
      + inferred computation type
      + equational constraints between types
      + equational constraints between rhos
      + inequational and eternality constraints *)
  and infer_computation state = function
    | Ast.Return expr ->
        let ty, ty_eqs, rho_eqs, rho_ineqs = infer_expression state expr in
        ( Ast.CompTy (ty, Ast.RhoConst ResourceGrade.zero),
          ty_eqs,
          rho_eqs,
          rho_ineqs )
    | Ast.Do (comp1, comp2) ->
        let CompTy (ty1, rho1), ty_eqs1, rho_eqs1, rho_ineqs1 =
          infer_computation state comp1
        in
        let comp_rho = fresh_rho () in
        let state' = extend_resource_grade state comp_rho in
        let ty1', Ast.CompTy (ty2, rho2), ty_eqs2, rho_eqs2, rho_ineqs2 =
          infer_abstraction state' comp2
        in
        ( CompTy (ty2, Ast.RhoAdd (comp_rho, rho2)),
          ((ty1, ty1') :: ty_eqs1) @ ty_eqs2,
          ((rho1, comp_rho) :: rho_eqs1) @ rho_eqs2,
          rho_ineqs1 @ rho_ineqs2 )
    | Ast.Apply (e1, e2) ->
        let t1, ty_eqs1, rho_eqs1, rho_ineqs1 = infer_expression state e1
        and t2, ty_eqs2, rho_eqs2, rho_ineqs2 = infer_expression state e2
        and a = fresh_comp_ty () in
        ( a,
          ((t1, Ast.TyArrow (t2, a)) :: ty_eqs1) @ ty_eqs2,
          rho_eqs1 @ rho_eqs2,
          rho_ineqs1 @ rho_ineqs2 )
    | Ast.Match (e, cases) ->
        let ty1, ty_eqs, rho_eqs, rho_ineqs = infer_expression state e
        and branch_comp_ty = fresh_comp_ty () in
        let (CompTy (branch_ty, branch_rho)) = branch_comp_ty in
        let fold (ty_eqs, rho_eqs, rho_ineqs) abs =
          let ( ty1',
                CompTy (branch_ty', branch_rho'),
                ty_eqs',
                rho_eqs',
                rho_ineqs' ) =
            infer_abstraction state abs
          in
          ( ((ty1, ty1') :: (branch_ty, branch_ty') :: ty_eqs') @ ty_eqs,
            ((branch_rho, branch_rho') :: rho_eqs') @ rho_eqs,
            rho_ineqs' @ rho_ineqs )
        in
        let ty_eqs'', rho_eqs'', rho_ineqs'' =
          List.fold_left fold (ty_eqs, rho_eqs, rho_ineqs) cases
        in
        (branch_comp_ty, ty_eqs'', rho_eqs'', rho_ineqs'')
    | Ast.Delay (n, c) ->
        let rho = Ast.RhoConst (ResourceGrade.of_nat n) in
        let state' = extend_resource_grade state rho in
        let CompTy (ty, rho'), ty_eqs, rho_eqs, rho_ineqs =
          infer_computation state' c
        in
        (CompTy (ty, Ast.RhoAdd (rho, rho')), ty_eqs, rho_eqs, rho_ineqs)
    | Ast.Box (rho, e, abs) ->
        let state_ahead = extend_resource_grade state rho in
        let value_ty, ty_eqs, rho_eqs, rho_ineqs =
          infer_expression state_ahead e
        in
        let value_ty', comp_ty, ty_eqs', rho_eqs', rho_ineqs' =
          infer_abstraction state abs
        in
        ( comp_ty,
          ((Ast.TyBox (rho, value_ty), value_ty') :: ty_eqs) @ ty_eqs',
          rho_eqs @ rho_eqs',
          rho_ineqs @ rho_ineqs' )
    | Ast.Unbox (e, abs) ->
        let rec findVar e =
          match e with
          | Ast.Var x -> x
          | Ast.Annotated (e', _) -> findVar e'
          | _ -> Error.typing "Unboxing requires a variable."
        in
        let x = findVar e in
        let ty_params, rho_params, constrs, ty, _var_type =
          ContextHolderModule.find_variable x state.variables
        in
        let ty_subst = refreshing_ty_subst ty_params in
        let rho_subst = refreshing_rho_subst rho_params in
        let boxed_ty = Ast.substitute_ty ty_subst rho_subst ty in
        let constrs' =
          instantiate_constrs ty_subst rho_subst (Ast.InstanceOf x) constrs
        in
        let value_ty, comp_ty, ty_eqs, rho_eqs, rho_ineqs =
          infer_abstraction state abs
        in
        let sum_rhos_added_after =
          ContextHolderModule.sum_rhos_added_after x state.variables
        in
        let rho = fresh_rho () in
        ( comp_ty,
          [ (Ast.TyBox (rho, value_ty), boxed_ty) ] @ ty_eqs,
          rho_eqs,
          (Ast.Ineq (sum_rhos_added_after, rho) :: constrs') @ rho_ineqs )
    | Ast.Perform (op, e, abs) -> (
        let op_sig = Ast.OpNameMap.find_opt op state.op_signatures in
        match op_sig with
        | None -> Error.typing "Unknown operation call."
        | Some (param_ty, arity_ty, op_rho) ->
            let value_ty, ty_eqs, rho_eqs, rho_ineqs =
              infer_expression state e
            in
            let state_ahead = extend_resource_grade state op_rho in
            let ( value_ty',
                  CompTy (cont_ty, cont_rho),
                  ty_eqs',
                  rho_eqs',
                  rho_ineqs' ) =
              infer_abstraction state_ahead abs
            in
            ( CompTy (cont_ty, Ast.RhoAdd (op_rho, cont_rho)),
              ((value_ty, param_ty) :: (value_ty', arity_ty) :: ty_eqs)
              @ ty_eqs',
              rho_eqs @ rho_eqs',
              rho_ineqs @ rho_ineqs' ))
    | Ast.Handle (c, h) ->
        let CompTy (ty, rho), ty_eqs, rho_eqs, rho_ineqs =
          infer_computation state c
        in
        let ty', ty_eqs', rho_eqs', rho_ineqs' = infer_expression state h in
        let ty'' = fresh_ty () in
        let rho'' = fresh_rho () in
        ( CompTy (ty'', Ast.RhoAdd (rho, rho'')),
          (ty', Ast.TyHandler (CompTy (ty, rho), CompTy (ty'', rho'')))
          :: ty_eqs
          @ ty_eqs',
          rho_eqs @ rho_eqs',
          rho_ineqs @ rho_ineqs' )

  and infer_abstraction state (pat, comp) =
    let ty, vars, ty_eqs = infer_pattern state pat in
    let state' = extend_local_variables state vars in
    let ty', ty_eqs', rho_eqs', rho_ineqs' = infer_computation state' comp in
    (ty, ty', ty_eqs @ ty_eqs', rho_eqs', rho_ineqs')

  let subst_ty_equations ty_subst rho_subst =
    let subst_ty_equation = function
      | t1, t2 ->
          ( Ast.substitute_ty ty_subst rho_subst t1,
            Ast.substitute_ty ty_subst rho_subst t2 )
    in
    List.map subst_ty_equation

  let subst_rho_equations rho_subst =
    let subst_rho_equation = function
      | rho1, rho2 ->
          (Ast.substitute_rho rho_subst rho1, Ast.substitute_rho rho_subst rho2)
    in
    List.map subst_rho_equation

  let subst_rho_inequations ty_subst rho_subst =
    List.map (Ast.substitute_constr ty_subst rho_subst)

  let add_ty_subst a ty ty_subst rho_subst =
    Ast.TyParamMap.add a (Ast.substitute_ty ty_subst rho_subst ty) ty_subst

  let add_rho_subst tp rho rho_subst =
    Ast.RhoParamMap.add tp (Ast.substitute_rho rho_subst rho) rho_subst

  let rec occurs_ty a = function
    | Ast.TyParam a' -> a = a'
    | Ast.TyConst _ -> false
    | Ast.TyArrow (ty1, CompTy (ty2, _)) -> occurs_ty a ty1 || occurs_ty a ty2
    | Ast.TyApply (_, tys) -> List.exists (occurs_ty a) tys
    | Ast.TyTuple tys -> List.exists (occurs_ty a) tys
    | Ast.TyBox (_, ty) -> occurs_ty a ty
    | Ast.TyHandler (CompTy (ty1, _), CompTy (ty2, _)) ->
        occurs_ty a ty1 || occurs_ty a ty2

  let rec occurs_rho a = function
    | Ast.RhoParam a' -> a = a'
    | Ast.RhoConst _ | Ast.RhoRigid _ -> false
    | Ast.RhoAdd (rho, rho') -> occurs_rho a rho || occurs_rho a rho'

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
        let rho_subst = refreshing_rho_subst [] in
        Ast.substitute_ty ty_subst rho_subst ty

  let rec simplify_rho rho =
    match rho with
    | Ast.RhoAdd (t1, t2) -> (
        let t1' = simplify_rho t1 in
        let t2' = simplify_rho t2 in
        match (t1', t2') with
        | Ast.RhoConst c1, Ast.RhoConst c2 ->
            Ast.RhoConst (ResourceGrade.add c1 c2)
        | (Ast.RhoConst z, t | t, Ast.RhoConst z) when z = ResourceGrade.zero ->
            t
        | _ -> Ast.RhoAdd (t1', t2'))
    | _ -> rho

  and simplify_ty ty =
    match ty with
    | Ast.TyConst t -> Ast.TyConst t
    | TyApply (ty_name, ty_list) ->
        TyApply (ty_name, List.map simplify_ty ty_list)
    | TyParam ty_param -> TyParam ty_param
    | TyArrow (ty, Ast.CompTy (ty', rho')) ->
        TyArrow (simplify_ty ty, Ast.CompTy (simplify_ty ty', simplify_rho rho'))
    | TyTuple ty_list -> TyTuple (List.map simplify_ty ty_list)
    | TyBox (rho, ty) -> TyBox (simplify_rho rho, simplify_ty ty)
    | TyHandler (Ast.CompTy (ty1, rho1), Ast.CompTy (ty2, rho2)) ->
        TyHandler
          ( Ast.CompTy (simplify_ty ty1, simplify_rho rho1),
            Ast.CompTy (simplify_ty ty2, simplify_rho rho2) )

  let simplify_comp_ty = function
    | Ast.CompTy (ty, rho) -> Ast.CompTy (simplify_ty ty, simplify_rho rho)

  let compare_rho a b =
    match (a, b) with
    | Either.Left p1, Either.Left p2 -> compare p1 p2 (* compare variables *)
    | Either.Right c1, Either.Right c2 -> compare c1 c2 (* compare constants *)
    | Either.Left _, _ -> -1
    | _, Either.Left _ -> 1

  (** [build_rho_param_list rho] lists the summands of [rho] from left to right,
      variables as [Either.Left] and constants as [Either.Right]. *)
  let build_rho_param_list rho =
    let rec aux acc rho =
      match rho with
      | (Ast.RhoParam _ | Ast.RhoRigid _) as v -> Either.Left v :: acc
      | Ast.RhoConst c -> Either.Right c :: acc
      | Ast.RhoAdd (rho1, rho2) ->
          let acc' = aux acc rho2 in
          aux acc' rho1
    in
    aux [] rho

  (** Drops the units and adds up the *adjacent* constants of a summand list.
      This is all a non-commutative grading monoid allows, and it is what makes
      a clause performing [delay 3; delay 4] realise the grade [7]. *)
  let rec fold_adjacent_constants = function
    | [] -> []
    | Either.Right c :: rest when c = ResourceGrade.zero ->
        fold_adjacent_constants rest
    | Either.Right c :: rest -> (
        match fold_adjacent_constants rest with
        | Either.Right c' :: rest' ->
            Either.Right (ResourceGrade.add c c') :: rest'
        | rest' -> Either.Right c :: rest')
    | (Either.Left _ as p) :: rest -> p :: fold_adjacent_constants rest

  (** Adds up *all* the constants of a summand list, keeping the parameters in
      their original order. Only sound when the grading monoid is commutative,
      since the constants need not be adjacent. *)
  let fold_all_constants params =
    let rest, total =
      List.fold_left
        (fun (rest, total) -> function
          | Either.Left _ as p -> (p :: rest, total)
          | Either.Right c -> (rest, ResourceGrade.add total c))
        ([], ResourceGrade.zero) params
    in
    let rest = List.rev rest in
    if total = ResourceGrade.zero then rest else rest @ [ Either.Right total ]

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

  let rec cancel_common_prefix left right =
    match (left, right) with
    | lhd :: ltl, rhd :: rtl when lhd = rhd -> cancel_common_prefix ltl rtl
    | _ -> (left, right)

  let cancel_common_suffix left right =
    let left', right' = cancel_common_prefix (List.rev left) (List.rev right) in
    (List.rev left', List.rev right')

  let build_rho_from_param_list params =
    let to_rho = function
      | Either.Left v -> v
      | Either.Right x -> Ast.RhoConst x
    in
    match params with
    | [] -> Ast.RhoConst ResourceGrade.zero
    | hd :: tl ->
        List.fold_left (fun acc e -> Ast.RhoAdd (acc, to_rho e)) (to_rho hd) tl

  (** [normalise_rho_pair rho1 rho2] rewrites a constraint between the two sums
      [rho1] and [rho2] into a simpler one that implies it: the summands are
      flattened left to right, the units are dropped, the adjacent constants are
      added up, and the summands the two sides have in common are cancelled.

      Cancelling is a sound but incomplete rule for a non-commutative grading
      monoid: from [a = b] we get [c · a = c · b] and [a · c = b · c] by
      congruence, and the same for [≾] by [·-monoˡ-≾] and [·-monoʳ-≾], so every
      solution of the cancelled constraint is a solution of the original one.
      The converse needs the monoid to be cancellative, which sets of timed
      traces are not: [{ε, a} · {ε, a, aa} = {ε, a} · {ε, aa}] even though
      [{ε, a, aa} ≠ {ε, aa}]. Consequently only the longest common prefix and
      the longest common suffix may be cancelled — those are the positions where
      what remains is still a single contiguous factor on both sides. When the
      monoid is commutative every summand may be moved to either end, so all the
      constants are added up, the summands are sorted, and any common summand is
      cancelled, which is what the typechecker has always done. *)
  let normalise_rho_pair rho1 rho2 =
    let left = fold_adjacent_constants (build_rho_param_list rho1) in
    let right = fold_adjacent_constants (build_rho_param_list rho2) in
    let left', right' =
      if ResourceGrade.is_commutative then
        cancel_common_elements
          (List.sort compare_rho (fold_all_constants left))
          (List.sort compare_rho (fold_all_constants right))
      else
        let left, right = cancel_common_prefix left right in
        cancel_common_suffix left right
    in
    (build_rho_from_param_list left', build_rho_from_param_list right')

  let rec unify_ty_constraints state rho_eqs = function
    | [] -> (Ast.TyParamMap.empty, rho_eqs)
    | (t1, t2) :: ty_eqs when t1 = t2 ->
        unify_ty_constraints state rho_eqs ty_eqs
    | (Ast.TyApply (ty_name1, args1), Ast.TyApply (ty_name2, args2)) :: ty_eqs
      when ty_name1 = ty_name2 ->
        let new_eqs =
          List.map (fun (t1, t2) -> (t1, t2)) (List.combine args1 args2)
        in
        unify_ty_constraints state rho_eqs (new_eqs @ ty_eqs)
    | (Ast.TyApply (ty_name, args), ty) :: ty_eqs
      when is_transparent_type state ty_name ->
        unify_ty_constraints state rho_eqs
          ((unfold state ty_name args, ty) :: ty_eqs)
    | (ty, Ast.TyApply (ty_name, args)) :: ty_eqs
      when is_transparent_type state ty_name ->
        unify_ty_constraints state rho_eqs
          ((ty, unfold state ty_name args) :: ty_eqs)
    | (Ast.TyTuple tys1, Ast.TyTuple tys2) :: ty_eqs
      when List.length tys1 = List.length tys2 ->
        let new_eqs =
          List.map (fun (t1, t2) -> (t1, t2)) (List.combine tys1 tys2)
        in
        unify_ty_constraints state rho_eqs (new_eqs @ ty_eqs)
    | ( Ast.TyArrow (t1, CompTy (t1', rho1')),
        Ast.TyArrow (t2, CompTy (t2', rho2')) )
      :: ty_eqs ->
        unify_ty_constraints state
          ((rho1', rho2') :: rho_eqs)
          ((t1, t2) :: (t1', t2') :: ty_eqs)
    | (Ast.TyParam a, t) :: ty_eqs when not (occurs_ty a t) ->
        let ty_subst, rho_eqs' =
          unify_ty_constraints state rho_eqs
            (subst_ty_equations
               (Ast.TyParamMap.singleton a t)
               Ast.RhoParamMap.empty ty_eqs)
        in
        (add_ty_subst a t ty_subst Ast.RhoParamMap.empty, rho_eqs')
    | (t, Ast.TyParam a) :: ty_eqs when not (occurs_ty a t) ->
        let ty_subst, rho_eqs' =
          unify_ty_constraints state rho_eqs
            (subst_ty_equations
               (Ast.TyParamMap.singleton a t)
               Ast.RhoParamMap.empty ty_eqs)
        in
        (add_ty_subst a t ty_subst Ast.RhoParamMap.empty, rho_eqs')
    | (Ast.TyBox (rho1, ty1), Ast.TyBox (rho2, ty2)) :: ty_eqs ->
        unify_ty_constraints state ((rho1, rho2) :: rho_eqs)
          ((ty1, ty2) :: ty_eqs)
    | ( Ast.TyHandler (Ast.CompTy (ty1, rho1), Ast.CompTy (ty2, rho2)),
        Ast.TyHandler (Ast.CompTy (ty1', rho1'), Ast.CompTy (ty2', rho2')) )
      :: ty_eqs ->
        unify_ty_constraints state
          ((rho1, rho1') :: (rho2, rho2') :: rho_eqs)
          ((ty1, ty1') :: (ty2, ty2') :: ty_eqs)
    | (t1, t2) :: _ ->
        let ty_pp = PrettyPrint.TyPrintParam.create () in
        let rho_pp = PrettyPrint.RhoPrintParam.create () in
        Error.typing "Cannot unify types %t = %t"
          (PrettyPrint.print_ty (module ResourceGrade) ty_pp rho_pp t1)
          (PrettyPrint.print_ty (module ResourceGrade) ty_pp rho_pp t2)

  let rec unify_rho_constraints state prev_unsolved_size unsolved = function
    | [] ->
        let current_unsolved_size = List.length unsolved in
        if current_unsolved_size = 0 then
          (* All constraints solved *)
          Ast.RhoParamMap.empty
        else if current_unsolved_size = prev_unsolved_size then
          Error.typing
            "Unification stuck - could not solve remaining constraints %t"
            (fun ppf ->
              print_rho_eq_constraints unsolved;
              Format.fprintf ppf "%s" "")
        else
          (* Retry with deferred constraints *)
          unify_rho_constraints state current_unsolved_size [] unsolved
    | (rho1, rho2) :: eqs -> (
        let rho1' = simplify_rho rho1 in
        let rho2' = simplify_rho rho2 in
        match (rho1', rho2') with
        | _ when rho1' = rho2' ->
            unify_rho_constraints state prev_unsolved_size unsolved eqs
        | Ast.RhoParam tp, rho when not (occurs_rho tp rho) ->
            let rho_subst =
              unify_rho_constraints state prev_unsolved_size
                (subst_rho_equations
                   (Ast.RhoParamMap.singleton tp rho)
                   unsolved)
                (subst_rho_equations (Ast.RhoParamMap.singleton tp rho) eqs)
            in
            add_rho_subst tp rho rho_subst
        | rho, Ast.RhoParam tp when not (occurs_rho tp rho) ->
            let rho_subst =
              unify_rho_constraints state prev_unsolved_size
                (subst_rho_equations
                   (Ast.RhoParamMap.singleton tp rho)
                   unsolved)
                (subst_rho_equations (Ast.RhoParamMap.singleton tp rho) eqs)
            in
            add_rho_subst tp rho rho_subst
        | Ast.RhoConst z, Ast.RhoAdd (t1, t2)
        | Ast.RhoAdd (t1, t2), Ast.RhoConst z
          when z = ResourceGrade.zero ->
            unify_rho_constraints state prev_unsolved_size unsolved
              ((t1, Ast.RhoConst ResourceGrade.zero)
              :: (t2, Ast.RhoConst ResourceGrade.zero)
              :: eqs)
        | t, (Ast.RhoAdd _ as u) ->
            let left_rho, right_rho = normalise_rho_pair t u in
            if left_rho = t && right_rho = u then
              unify_rho_constraints state prev_unsolved_size
                ((left_rho, right_rho) :: unsolved)
                eqs
            else
              unify_rho_constraints state prev_unsolved_size unsolved
                ((left_rho, right_rho) :: eqs)
        | (Ast.RhoAdd _ as u), t ->
            let left_rho, right_rho = normalise_rho_pair u t in
            if left_rho = u && right_rho = t then
              unify_rho_constraints state prev_unsolved_size
                ((left_rho, right_rho) :: unsolved)
                eqs
            else
              unify_rho_constraints state prev_unsolved_size unsolved
                ((left_rho, right_rho) :: eqs)
        | (Ast.RhoRigid _ as r), u | u, (Ast.RhoRigid _ as r) ->
            (* Nothing above applied, so the other side is neither an unknown
               nor a reducible sum: the case would be well-typed only for this
               one grade of its continuation. *)
            let rho_pp = PrettyPrint.RhoPrintParam.create () in
            Error.typing
              "The grade %t of a handler continuation may be any grade, but \
               here it is required to equal %t"
              (PrettyPrint.print_rho (module ResourceGrade) rho_pp r)
              (PrettyPrint.print_rho (module ResourceGrade) rho_pp u)
        | u1, u2 ->
            unify_rho_constraints state prev_unsolved_size
              ((u1, u2) :: unsolved) eqs)

  (** [reduce_eternal state ty] reduces the obligation that [ty] be eternal to
      the type variables it depends on: [None] when [ty] is not eternal whatever
      its variables are instantiated with, and [Some vars] when it is eternal
      exactly if every variable in [vars] is, so [Some empty] when it is eternal
      outright.

      A type application is reduced by unfolding its definition with the
      arguments substituted in, so that only the parameters that actually occur
      in a constructor argument, and occur there outside of a function, handler
      or box type, impose an obligation. [visited] tracks the type names
      currently being unfolded to stop the recursion on recursive types (e.g.
      list): a recursive occurrence is assumed eternal coinductively, since it
      is guarded and its non-recursive parts are checked by the outer call. A
      [noneternal] declaration overrides the structural check, and does so
      before [visited] is consulted, so that a noneternal type occurring inside
      a recursive type poisons it too. *)
  let reduce_eternal state ty =
    let ( let* ) = Option.bind in
    let rec all visited tys =
      List.fold_left
        (fun acc ty ->
          let* acc = acc in
          let* vars = check visited ty in
          Some (Ast.TyParamSet.union acc vars))
        (Some Ast.TyParamSet.empty) tys
    and check visited = function
      | Ast.TyConst c ->
          if Const.is_eternal_ty c then Some Ast.TyParamSet.empty else None
      | Ast.TyParam a -> Some (Ast.TyParamSet.singleton a)
      | Ast.TyArrow _ | Ast.TyBox _ | Ast.TyHandler _ -> None
      | Ast.TyTuple tys -> all visited tys
      | Ast.TyApply (ty_name, args) -> (
          if Ast.TyNameSet.mem ty_name state.noneternal_types then None
          else if List.mem ty_name visited then Some Ast.TyParamSet.empty
          else
            match Ast.TyNameMap.find_opt ty_name state.type_definitions with
            | None -> None
            | Some (params, ty_def) -> (
                let ty_subst =
                  List.fold_left2
                    (fun subst param arg -> Ast.TyParamMap.add param arg subst)
                    Ast.TyParamMap.empty params args
                in
                let subst = Ast.substitute_ty ty_subst Ast.RhoParamMap.empty in
                let visited' = ty_name :: visited in
                match ty_def with
                | Ast.TyInline ty -> check visited' (subst ty)
                | Ast.TySum variants ->
                    all visited'
                      (List.filter_map
                         (fun (_, arg_ty) -> Option.map subst arg_ty)
                         variants)))
    in
    check [] ty

  let rec unify_rho_ineq_constraints state prev_unsolved_size unsolved =
    let process ~may_default wrap rho1 rho2 ineqs =
      let rho1' = simplify_rho rho1 in
      let rho2' = simplify_rho rho2 in
      match (rho1', rho2') with
      | _ when rho1' = rho2' ->
          unify_rho_ineq_constraints state prev_unsolved_size unsolved ineqs
      (* [ρ ≾ 0] with [ρ] still unknown: when the unit is the top of the
         sub-grade order the constraint holds outright and is discharged by
         [simplify_constraints], so leave it alone; otherwise take [ρ := 0],
         which satisfies the constraint by reflexivity. That is the unique
         solution when the unit is minimal, and a sound — if possibly
         incomplete — default when it is neither minimal nor top, as for the
         two-sided timed-trace grade. A disjunction [eternal τ ∨ ρ ≾ 0] is
         defaulted only when [τ] cannot be eternal; otherwise it is left for
         the eternality of [τ] to decide, possibly in the scheme of the
         definition. *)
      | Ast.RhoParam tp, rho
        when may_default
             && (not (occurs_rho tp rho))
             && rho = Ast.RhoConst ResourceGrade.zero
             && not ResourceGrade.is_zero_top_sub_rho ->
          let singleton = Ast.RhoParamMap.singleton tp rho in
          let rho_subst, unsolved' =
            unify_rho_ineq_constraints state prev_unsolved_size
              (subst_rho_inequations Ast.TyParamMap.empty singleton unsolved)
              (subst_rho_inequations Ast.TyParamMap.empty singleton ineqs)
          in
          (add_rho_subst tp rho rho_subst, unsolved')
      | t, (Ast.RhoAdd _ as u) ->
          let left_rho, right_rho = normalise_rho_pair t u in
          if left_rho = t && right_rho = u then
            unify_rho_ineq_constraints state prev_unsolved_size
              (wrap left_rho right_rho :: unsolved)
              ineqs
          else
            unify_rho_ineq_constraints state prev_unsolved_size unsolved
              (wrap left_rho right_rho :: ineqs)
      | (Ast.RhoAdd _ as u), t ->
          let left_rho, right_rho = normalise_rho_pair u t in
          if left_rho = u && right_rho = t then
            unify_rho_ineq_constraints state prev_unsolved_size
              (wrap left_rho right_rho :: unsolved)
              ineqs
          else
            unify_rho_ineq_constraints state prev_unsolved_size unsolved
              (wrap left_rho right_rho :: ineqs)
      | rho1'', rho2'' ->
          unify_rho_ineq_constraints state prev_unsolved_size
            (wrap rho1'' rho2'' :: unsolved)
            ineqs
    in
    function
    | [] ->
        let current_unsolved_size = List.length unsolved in
        if current_unsolved_size = prev_unsolved_size then
          (Ast.RhoParamMap.empty, unsolved)
        else
          (* Retry with deferred constraints *)
          unify_rho_ineq_constraints state current_unsolved_size [] unsolved
    | Ast.EternalOrIneq (ty, rho1, rho2, origin) :: ineqs ->
        process
          ~may_default:(reduce_eternal state ty = None)
          (fun r1 r2 -> Ast.EternalOrIneq (ty, r1, r2, origin))
          rho1 rho2 ineqs
    | Ast.Ineq (rho1, rho2) :: ineqs ->
        process ~may_default:true
          (fun r1 r2 -> Ast.Ineq (r1, r2))
          rho1 rho2 ineqs
    | (Ast.Eternal _ as c) :: ineqs ->
        unify_rho_ineq_constraints state prev_unsolved_size (c :: unsolved)
          ineqs

  (** The cost model the sub-grade order of the timed-trace grades reads: the
      runtime bounds declared by the operation named by a trace event. Only the
      grades of operation signatures have their events validated when they are
      declared (see {!add_operation_signature}); the events of any other grade
      literal — a [box] grade or a grade written in a type — are checked lazily,
      here, the first time the order needs their cost. *)
  let op_bounds state ev =
    match StringMap.find_opt ev state.op_bounds with
    | Some bounds -> bounds
    | None ->
        Error.typing
          "Unknown event '%s'; the events of a resource grade must be declared \
           operations"
          ev

  let origin_subject = function
    | Ast.UseOf x -> Format.asprintf " of variable %t" (Ast.Variable.print x)
    | Ast.InstanceOf _ -> ""

  let origin_reason = function
    | Ast.UseOf _ -> ""
    | Ast.InstanceOf x ->
        Format.asprintf ", as required by the type of %t" (Ast.Variable.print x)

  (** The verdict on an inequality [ρ₁ ≾ ρ₂]. Rigid grades in it are universally
      quantified, so [Holds] means it holds for every grade they may take, and
      [Fails (Some w)] that it already fails when they are all [w]. [Unknown] is
      left open by the unknown grades. *)
  type verdict = Holds | Fails of ResourceGrade.t option | Unknown

  (** [decide_ineq state rho1 rho2] decides [rho1 ≾ rho2] as far as the unknown
      grades allow, never guessing at them. To prove it, {!normalise_rho_pair}
      cancels the common prefix and suffix, and the rigid grades are dropped
      from the side where that only strengthens the inequality: the greater side
      when the unit is the minimum of the order, the smaller side when it is the
      top. If what remains is ground, it decides. To refute it, the rigid grades
      are instantiated by a witness — the unit, one tick, or one tick past all
      the constants — since a failing ground instance refutes the universal
      statement. *)
  let decide_ineq state rho1 rho2 =
    let eval rho =
      try Some (ContextHolderModule.eval_rho rho)
      with Exception.RhoParamInEval _ -> None
    in
    (* Whether a ground [rho1 ≾ rho2] holds. The greater side is evaluated
       first: when it is the unit and the unit is the top of the order, the
       inequality holds whatever the smaller side is, and dually. *)
    let decide_ground rho1 rho2 =
      match eval rho2 with
      | Some v2
        when v2 = ResourceGrade.zero && ResourceGrade.is_zero_top_sub_rho ->
          Some true
      | v2 -> (
          match (eval rho1, v2) with
          | Some v1, _
            when v1 = ResourceGrade.zero
                 && ResourceGrade.is_zero_minimal_sub_rho ->
              Some true
          | Some v1, Some v2 ->
              Some (ResourceGrade.is_sub_rho (op_bounds state) v1 v2)
          | _ -> None)
    in
    let strip_rigid rho =
      build_rho_param_list rho
      |> List.filter (function
        | Either.Left (Ast.RhoRigid _) -> false
        | _ -> true)
      |> build_rho_from_param_list
    in
    let rec instantiate_rigid w = function
      | Ast.RhoRigid _ -> Ast.RhoConst w
      | Ast.RhoAdd (l, r) ->
          Ast.RhoAdd (instantiate_rigid w l, instantiate_rigid w r)
      | rho -> rho
    in
    if rho1 = rho2 then Holds
    else
      let left, right = normalise_rho_pair rho1 rho2 in
      let left' =
        if ResourceGrade.is_zero_top_sub_rho then strip_rigid left else left
      and right' =
        if ResourceGrade.is_zero_minimal_sub_rho then strip_rigid right
        else right
      in
      match decide_ground left' right' with
      | Some true -> Holds
      | Some false -> Fails None
      | None -> (
          if
            Ast.RhoParamSet.is_empty (Ast.rigid_rhos left)
            && Ast.RhoParamSet.is_empty (Ast.rigid_rhos right)
          then Unknown
          else
            let refutes w =
              decide_ground (instantiate_rigid w left)
                (instantiate_rigid w right)
              = Some false
            in
            (* One tick past all the constants refutes the bound a constant
               puts on a rigid grade, such as [ρ ≾ 1] under an upper bound. *)
            let beyond =
              List.fold_left
                (fun acc -> function
                  | Either.Right c -> ResourceGrade.add acc c
                  | Either.Left _ -> acc)
                (ResourceGrade.of_nat 1)
                (build_rho_param_list left @ build_rho_param_list right)
            in
            match
              List.find_opt refutes
                [ ResourceGrade.zero; ResourceGrade.of_nat 1; beyond ]
            with
            | Some w -> Fails (Some w)
            | None -> Unknown)

  (** [simplify_constraints state constrs] discharges the constraints that hold,
      fails on those that cannot, and returns the residue left open by the still
      unknown parameters.

      Inequalities are decided by {!decide_ineq}. An eternality obligation is
      reduced by {!reduce_eternal} to the type variables it depends on, and is
      discharged when there are none. A disjunction [eternal τ ∨ ρ₁ ≾ ρ₂] is
      discharged as soon as either side holds, and reduced to [eternal τ] when
      the inequality fails or a rigid grade leaves it open, since nothing may be
      assumed about the grade of a continuation. *)
  let simplify_constraints state constrs =
    let rho_pp = PrettyPrint.RhoPrintParam.create () in
    let ty_pp = PrettyPrint.TyPrintParam.create () in
    let print_rho rho ppf =
      PrettyPrint.print_rho (module ResourceGrade) rho_pp rho ppf
    in
    let print_ty ty ppf =
      PrettyPrint.print_ty (module ResourceGrade) ty_pp rho_pp ty ppf
    in
    let mentions_rigid rho =
      not (Ast.RhoParamSet.is_empty (Ast.rigid_rhos rho))
    in
    let print_witness rho1 rho2 witness ppf =
      match witness with
      | None -> ()
      | Some w ->
          let rigid =
            Ast.RhoParamSet.union (Ast.rigid_rhos rho1) (Ast.rigid_rhos rho2)
          in
          Format.fprintf ppf ", already when the continuation grade%s %s %s"
            (if Ast.RhoParamSet.cardinal rigid > 1 then "s" else "")
            (if Ast.RhoParamSet.cardinal rigid > 1 then "are" else "is")
            (ResourceGrade.show w)
    in
    let simplify acc = function
      | Ast.Ineq (rho1, rho2) -> (
          let rho1 = simplify_rho rho1 and rho2 = simplify_rho rho2 in
          match decide_ineq state rho1 rho2 with
          | Holds -> acc
          | Fails witness ->
              Error.typing "Comparing resource inequality %t %s %t failed%t"
                (print_rho rho1) ResourceGrade.is_sub_rho_symbol
                (print_rho rho2)
                (print_witness rho1 rho2 witness)
          | Unknown -> Ast.Ineq (rho1, rho2) :: acc)
      | Ast.Eternal (ty, origin) as c -> (
          match reduce_eternal state ty with
          | None ->
              Error.typing "Type %t%s is not eternal%s" (print_ty ty)
                (origin_subject origin) (origin_reason origin)
          | Some vars when Ast.TyParamSet.is_empty vars -> acc
          | Some _ -> c :: acc)
      | Ast.EternalOrIneq (ty, rho1, rho2, origin) -> (
          let rho1 = simplify_rho rho1 and rho2 = simplify_rho rho2 in
          match decide_ineq state rho1 rho2 with
          | Holds -> acc
          | verdict -> (
              match (reduce_eternal state ty, verdict) with
              | Some vars, _ when Ast.TyParamSet.is_empty vars -> acc
              | None, Fails witness ->
                  Error.typing
                    "Type %t%s is not eternal and resource inequality %t %s %t \
                     failed%t%s"
                    (print_ty ty) (origin_subject origin) (print_rho rho1)
                    ResourceGrade.is_sub_rho_symbol (print_rho rho2)
                    (print_witness rho1 rho2 witness)
                    (origin_reason origin)
              | None, _ ->
                  Error.typing
                    "Type %t%s is not eternal and cannot compare non-ground \
                     resource values %t and %t%s"
                    (print_ty ty) (origin_subject origin) (print_rho rho1)
                    (print_rho rho2) (origin_reason origin)
              | Some _, Fails _ -> Ast.Eternal (ty, origin) :: acc
              | Some _, _ ->
                  if mentions_rigid rho1 || mentions_rigid rho2 then
                    Ast.Eternal (ty, origin) :: acc
                  else Ast.EternalOrIneq (ty, rho1, rho2, origin) :: acc))
    in
    List.rev (List.fold_left simplify [] constrs)

  (** [solve_residuals state ~generalisable constrs] turns the residue left by
      {!simplify_constraints} into the qualifier of a type scheme whose
      quantified parameters are [generalisable]. A parameter of the residue that
      is not generalisable, so that it occurs in no type the definition exports,
      is ambiguous and may be instantiated freely: the type variables are taken
      to be [unit] and the grade parameters zero, which satisfies every
      eternality obligation on them and every [ρ ≾ 0]. What remains is then
      either an inequality that still is not ground, which is rejected as
      inequalities are not carried into schemes, or an eternality constraint on
      generalisable variables, which is put into canonical form: a conjunction
      of obligations on single variables, a disjunction over the tuple of the
      variables its type reduces to, and no constraint implied by another. *)
  let solve_residuals state ~generalisable:(gen_tys, gen_rhos) constrs =
    let fv_tys, fv_rhos =
      List.fold_left
        (fun (tys, rhos) c ->
          let tys', rhos' = Ast.free_vars_constr c in
          (Ast.TyParamSet.union tys tys', Ast.RhoParamSet.union rhos rhos'))
        (Ast.TyParamSet.empty, Ast.RhoParamSet.empty)
        constrs
    in
    let ty_subst =
      Ast.TyParamSet.fold
        (fun a subst -> Ast.TyParamMap.add a (Ast.TyTuple []) subst)
        (Ast.TyParamSet.diff fv_tys gen_tys)
        Ast.TyParamMap.empty
    in
    let rho_subst =
      Ast.RhoParamSet.fold
        (fun r subst ->
          Ast.RhoParamMap.add r (Ast.RhoConst ResourceGrade.zero) subst)
        (Ast.RhoParamSet.diff fv_rhos gen_rhos)
        Ast.RhoParamMap.empty
    in
    let constrs' =
      simplify_constraints state
        (List.map (Ast.substitute_constr ty_subst rho_subst) constrs)
    in
    let vars_of ty =
      match reduce_eternal state ty with
      | Some vars -> Ast.TyParamSet.elements vars
      | None -> assert false (* [simplify_constraints] has rejected it *)
    in
    let canonical_ty = function
      | [ a ] -> Ast.TyParam a
      | vars -> Ast.TyTuple (List.map (fun a -> Ast.TyParam a) vars)
    in
    let eternal_atoms, disjunctions =
      List.fold_left
        (fun (atoms, disjs) c ->
          match c with
          | Ast.Ineq (rho1, rho2) ->
              let rho_pp = PrettyPrint.RhoPrintParam.create () in
              let print_rho rho ppf =
                PrettyPrint.print_rho (module ResourceGrade) rho_pp rho ppf
              in
              let rigid =
                Ast.RhoParamSet.union (Ast.rigid_rhos rho1)
                  (Ast.rigid_rhos rho2)
              in
              Error.typing
                "Cannot compare non-ground resource values %t and %t%t"
                (print_rho rho1) (print_rho rho2) (fun ppf ->
                  match Ast.RhoParamSet.elements rigid with
                  | [] -> ()
                  | [ r ] ->
                      Format.fprintf ppf
                        ", where %t is the grade of a handler continuation and \
                         may be any grade"
                        (print_rho (Ast.RhoRigid r))
                  | rs ->
                      Format.fprintf ppf
                        ", where %t are grades of handler continuations and \
                         may be any grades" (fun ppf ->
                          Format.pp_print_list
                            ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
                            (fun ppf r -> print_rho (Ast.RhoRigid r) ppf)
                            ppf rs))
          | Ast.Eternal (ty, origin) ->
              ( List.fold_left
                  (fun atoms a ->
                    if List.mem_assoc a atoms then atoms
                    else atoms @ [ (a, origin) ])
                  atoms (vars_of ty),
                disjs )
          | Ast.EternalOrIneq (ty, rho1, rho2, origin) ->
              let vars = vars_of ty in
              if
                List.exists
                  (function
                    | Ast.EternalOrIneq (ty', rho1', rho2', _) ->
                        ty' = canonical_ty vars && rho1' = rho1 && rho2' = rho2
                    | _ -> false)
                  disjs
              then (atoms, disjs)
              else
                ( atoms,
                  disjs
                  @ [
                      Ast.EternalOrIneq (canonical_ty vars, rho1, rho2, origin);
                    ] ))
        ([], []) constrs'
    in
    let implied = function
      | Ast.EternalOrIneq (ty, _, _, _) ->
          List.for_all (fun a -> List.mem_assoc a eternal_atoms) (vars_of ty)
      | _ -> false
    in
    List.map
      (fun (a, origin) -> Ast.Eternal (Ast.TyParam a, origin))
      eternal_atoms
    @ List.filter (fun c -> not (implied c)) disjunctions

  let unify state ty_eqs rho_eqs rho_ineqs =
    let ty_subst, rho_eqs' = unify_ty_constraints state [] ty_eqs in
    let rho_subst = unify_rho_constraints state 0 [] (rho_eqs @ rho_eqs') in
    let rho_ineqs' = subst_rho_inequations ty_subst rho_subst rho_ineqs in
    let rho_subst', rho_ineqs'' =
      unify_rho_ineq_constraints state 0 [] rho_ineqs'
    in
    let rho_subst'' =
      Ast.RhoParamMap.union
        (fun _ v _ -> Some v)
        (Ast.RhoParamMap.map
           (fun rho -> Ast.substitute_rho rho_subst' rho)
           rho_subst)
        rho_subst'
    in
    let residual =
      simplify_constraints state
        (subst_rho_inequations ty_subst rho_subst'' rho_ineqs'')
    in
    let ty_subst' =
      Ast.TyParamMap.map
        (fun ty -> Ast.substitute_ty ty_subst rho_subst'' ty)
        ty_subst
    in
    (ty_subst', rho_subst'', residual)

  (** A rigid grade is universally quantified in the handler case that
      introduced it, so it must not escape: in the type of a definition it would
      be generalised and instantiated freely at each use. *)
  let check_no_rigid_escape rigid describe =
    match Ast.RhoParamSet.choose_opt rigid with
    | None -> ()
    | Some r ->
        let rho_pp = PrettyPrint.RhoPrintParam.create () in
        let ty_pp = PrettyPrint.TyPrintParam.create () in
        Error.typing
          "The grade %t of a handler continuation may be any grade and cannot \
           occur in %t"
          (PrettyPrint.print_rho (module ResourceGrade) rho_pp (Ast.RhoRigid r))
          (describe ty_pp rho_pp)

  let infer state e =
    let comp_ty, ty_eqs, rho_eqs, rho_ineqs = infer_computation state e in
    let ty_subst, rho_subst, residual = unify state ty_eqs rho_eqs rho_ineqs in
    (* A top-level computation exports no type, so every parameter of its
       residual constraints may be instantiated as the constraints need. *)
    let _ =
      solve_residuals state
        ~generalisable:(Ast.TyParamSet.empty, Ast.RhoParamSet.empty)
        residual
    in
    let comp_ty' =
      simplify_comp_ty (Ast.substitute_comp_ty ty_subst rho_subst comp_ty)
    in
    (let (Ast.CompTy (ty, rho)) = comp_ty' in
     check_no_rigid_escape (Ast.rigid_rhos_comp_ty comp_ty')
       (fun ty_pp rho_pp ppf ->
         Format.fprintf ppf "the type %t # %t of the computation"
           (PrettyPrint.print_ty (module ResourceGrade) ty_pp rho_pp ty)
           (PrettyPrint.print_rho (module ResourceGrade) rho_pp rho)));
    comp_ty'

  let add_external_function x ty_sch state =
    {
      state with
      variables = ContextHolderModule.add_variable x ty_sch state.variables;
    }

  let add_top_definition state x e =
    (* Format.fprintf Format.std_formatter "\n";
    PrettyPrint.print_expression (module ResourceGrade) e Format.std_formatter;
    Format.fprintf Format.std_formatter "\n"; *)
    let ty, ty_eqs, rho_eqs, rho_ineqs = infer_expression state e in
    let ty_subst, rho_subst, residual = unify state ty_eqs rho_eqs rho_ineqs in
    let ty' = Ast.substitute_ty ty_subst rho_subst ty in
    let ty'' = simplify_ty ty' in
    check_no_rigid_escape (Ast.rigid_rhos_ty ty'') (fun ty_pp rho_pp ppf ->
        Format.fprintf ppf "the type %t of %t"
          (PrettyPrint.print_ty (module ResourceGrade) ty_pp rho_pp ty'')
          (Ast.Variable.print x));
    let free_vars, free_rhos = Ast.free_vars ty'' in
    (* The constraints the definition could not discharge qualify its scheme,
       to be owed again at each use. *)
    let constrs =
      solve_residuals state ~generalisable:(free_vars, free_rhos) residual
    in
    let ty_sch =
      ( free_vars |> Ast.TyParamSet.elements,
        free_rhos |> Ast.RhoParamSet.elements,
        constrs,
        ty'',
        Global )
    in
    add_external_function x ty_sch state

  (* An alias is unfolded by the unifier before the eternality check ever sees
     it, so a [noneternal] flag on one could not be honoured; reject it rather
     than silently ignore it. *)
  let check_noneternal_definable (_, ty_name, ty_def) =
    match ty_def with
    | Ast.TySum _ -> ()
    | Ast.TyInline _ ->
        let name = Ast.TyName.string_of ty_name in
        Error.typing
          "type %s is an alias and cannot be declared noneternal; wrap it in a \
           constructor, as in 'noneternal type %s = %s of ...'"
          name name
          (String.capitalize_ascii name)

  let add_type_definitions state (eternality, ty_defs) =
    (match eternality with
    | Ast.Derived -> ()
    | Ast.Noneternal -> List.iter check_noneternal_definable ty_defs);
    let state' =
      List.fold_left
        (fun state (params, ty_name, ty_def) ->
          {
            state with
            type_definitions =
              Ast.TyNameMap.add ty_name (params, ty_def) state.type_definitions;
            noneternal_types =
              (match eternality with
              | Ast.Derived -> state.noneternal_types
              | Ast.Noneternal ->
                  Ast.TyNameSet.add ty_name state.noneternal_types);
          })
        state ty_defs
    in
    List.iter (fun (_, _, ty_def) -> check_ty_def state' ty_def) ty_defs;
    state'

  (* Under the timed-trace grading monoids an operation's runtime bounds
     [within (lo, hi)] are the cost model the orders read. An atomic operation,
     graded by the single run of itself, has to declare them, since nothing
     else says how long it takes. A compound operation names the operations it
     decomposes into, and its bounds follow from theirs: [lo] is the duration
     of the fastest run of its grade with every event at its lower bound and
     [hi] that of the slowest run with every event at its upper bound.
     Declaring them as well would only
     invite disagreement, so it is rejected. A compound operation may not name
     itself, since its bounds would then depend on themselves. *)
  let add_operation_signature state (op, ty1, ty2, rho, bounds) =
    let op_name = Ast.OpName.string_of op in
    let event_bounds ev =
      match StringMap.find_opt ev state.op_bounds with
      | Some bounds -> bounds
      | None ->
          Error.typing "unknown event '%s' in the grade of operation %s" ev
            op_name
    in
    let op_bounds' =
      match (ResourceGrade.needs_op_bounds, rho, bounds) with
      | false, _, Some _ ->
          Error.typing
            "runtime bounds are only used by the timed-trace grading monoids; \
             under '%s' the operation grade already carries them"
            ResourceGrade.name
      | false, _, None -> state.op_bounds
      | true, (Ast.RhoParam _ | Ast.RhoRigid _ | Ast.RhoAdd _), _ ->
          Error.typing
            "the grade of operation %s must be a literal under the '%s' \
             grading monoid"
            op_name ResourceGrade.name
      | true, Ast.RhoConst grade, _ when ResourceGrade.is_atomic op_name grade
        -> (
          match bounds with
          | None ->
              Error.typing
                "atomic operation %s needs runtime bounds `within (lo, hi)` \
                 under the '%s' grading monoid"
                op_name ResourceGrade.name
          | Some (lo, hi) ->
              if lo > hi then
                Error.typing
                  "the runtime bounds of operation %s must satisfy lo <= hi"
                  op_name
              else if hi < 1 then
                Error.typing
                  "the upper runtime bound of operation %s must be at least 1"
                  op_name
              else StringMap.add op_name (lo, hi) state.op_bounds)
      | true, Ast.RhoConst grade, Some _ ->
          Error.typing
            "operation %s is compound, so its runtime bounds follow from its \
             grade %s and must not be declared"
            op_name (ResourceGrade.show grade)
      | true, Ast.RhoConst grade, None -> (
          if List.mem op_name (ResourceGrade.events grade) then
            Error.typing
              "compound operation %s may not name itself in its grade %s"
              op_name (ResourceGrade.show grade);
          match ResourceGrade.implied_bounds event_bounds grade with
          | Some bounds -> StringMap.add op_name bounds state.op_bounds
          | None -> state.op_bounds)
    in
    {
      state with
      op_signatures = Ast.OpNameMap.add op (ty1, ty2, rho) state.op_signatures;
      op_bounds = op_bounds';
    }

  (* A default implementation cannot be checked the way an operation case of a
     handler is: a case for [Op] may spend the grade of [Op] itself, because the
     operation it handles has already been performed, while a default *is* the
     operation and has nothing to spend. So it is checked against the time the
     operation is allowed to take instead, its runtime bounds
     [within (lo, hi)] read as a grade by [ResourceGrade.of_bounds]. Under the
     time grading monoids the grade of an operation already is its runtime
     bound, so there the operation's own grade is the bound to check against.
     Under the trace grades a default is moreover only meaningful for an atomic
     operation: a compound one names the operations it decomposes into and is
     to be given meaning by a handler in terms of them. *)
  let add_operation_default state (op, abs) =
    let op_name = Ast.OpName.string_of op in
    match Ast.OpNameMap.find_opt op state.op_signatures with
    | None -> Error.typing "unknown operation %s" op_name
    | Some (param_ty, arity_ty, op_rho) ->
        if Ast.OpNameSet.mem op state.op_defaults then
          Error.typing "operation %s already has a default implementation"
            op_name;
        (match op_rho with
        | Ast.RhoConst grade when not (ResourceGrade.is_atomic op_name grade) ->
            Error.typing
              "a default implementation may only be given for an atomic \
               operation, but the grade of %s is %s; handle it with a handler \
               in terms of the operations it names"
              op_name (ResourceGrade.show grade)
        | Ast.RhoConst _ | Ast.RhoParam _ | Ast.RhoRigid _ | Ast.RhoAdd _ -> ());
        let bound_rho =
          match StringMap.find_opt op_name state.op_bounds with
          | Some bounds -> Ast.RhoConst (ResourceGrade.of_bounds bounds)
          | None -> op_rho
        in
        let arg_ty, CompTy (res_ty, impl_rho), ty_eqs, rho_eqs, rho_ineqs =
          infer_abstraction state abs
        in
        let _, _, residual =
          unify state
            ((arg_ty, param_ty) :: (res_ty, arity_ty) :: ty_eqs)
            rho_eqs
            (Ast.Ineq (impl_rho, bound_rho) :: rho_ineqs)
        in
        let _ =
          solve_residuals state
            ~generalisable:(Ast.TyParamSet.empty, Ast.RhoParamSet.empty)
            residual
        in
        { state with op_defaults = Ast.OpNameSet.add op state.op_defaults }

  let load_primitive state x prim =
    let ty_params, rho_params, ty = P.primitive_type_scheme prim in
    add_external_function x (ty_params, rho_params, [], ty, Global) state
end
