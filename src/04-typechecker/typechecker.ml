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
      * ResourceGrade.t Ast.ty
      * var_type)
      ContextHolderModule.t;
    type_definitions :
      (Ast.ty_param list * ResourceGrade.t Ast.ty_def) Ast.TyNameMap.t;
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
      op_bounds = StringMap.empty;
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

  let print_fresh_rho_constraint rho rho_pp =
    Format.printf "FreshRhoConstraint(%t)"
      (PrettyPrint.print_rho (module ResourceGrade) rho_pp rho)

  let print_one_fresh_rho_constraint rho =
    let rho_pp = PrettyPrint.RhoPrintParam.create () in
    print_fresh_rho_constraint rho rho_pp

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

  type ineq_constraint =
    | Ineq of ContextHolderModule.base_rho * ResourceGrade.t Ast.rho
    | EternalOrIneq of
        ResourceGrade.t Ast.ty
        * ContextHolderModule.base_rho
        * ResourceGrade.t Ast.rho

  let print_rho_ineq_constraints_pp rho_pp constraints =
    Format.fprintf Format.std_formatter "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
         (fun _ppf constraint_ ->
           match constraint_ with
           | Ineq (rho1, rho2) -> print_rho_geq rho1 rho2 rho_pp
           | EternalOrIneq (ty, rho1, rho2) ->
               let ty_pp = PrettyPrint.TyPrintParam.create () in
               Format.printf "%t ∨ (%t %s %t)"
                 (PrettyPrint.print_ty (module ResourceGrade) ty_pp rho_pp ty)
                 (PrettyPrint.print_rho (module ResourceGrade) rho_pp rho1)
                 ResourceGrade.is_sub_rho_symbol
                 (PrettyPrint.print_rho (module ResourceGrade) rho_pp rho2)))
      constraints

  let print_rho_ineq_constraints constraints =
    let rho_pp = PrettyPrint.RhoPrintParam.create () in
    print_rho_ineq_constraints_pp rho_pp constraints

  let print_rho_abs_constraints_pp rho_pp constraints =
    Format.fprintf Format.std_formatter "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
         (fun _ppf constraint_ ->
           match constraint_ with rho -> print_fresh_rho_constraint rho rho_pp))
      constraints

  let print_rho_abs_constraints constraints =
    let rho_pp = PrettyPrint.RhoPrintParam.create () in
    print_rho_abs_constraints_pp rho_pp constraints

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
          ContextHolderModule.add_variable x ([], [], ty, Local) state.variables
        in
        { state with variables = updated_variables })
      state vars

  let extend_global_variables state vars =
    List.fold_left
      (fun state (x, ty) ->
        let updated_variables =
          ContextHolderModule.add_variable x ([], [], ty, Global)
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
      + inequational constraints between rhos
      + list of abstractly quantified rhos (in operation cases of effect
        handlers) *)
  let rec infer_expression state = function
    | Ast.Var x ->
        let ty_params, rho_params, ty, var_type =
          ContextHolderModule.find_variable x state.variables
        in
        let rho_ineq =
          match var_type with
          | Local ->
              [
                EternalOrIneq
                  ( ty,
                    ContextHolderModule.sum_rhos_added_after x state.variables,
                    Ast.RhoConst ResourceGrade.zero );
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
        (Ast.substitute_ty ty_subst rho_subst ty, [], [], rho_ineq, [])
        (* type, type constraints, rho constraints, 
           rho inequational constraints, rho abstractness constraints *)
    | Ast.Const c -> (Ast.TyConst (Const.infer_ty c), [], [], [], [])
    | Ast.Annotated (expr, ty) ->
        let ty', ty_eqs, rho_eqs, rho_ineqs, rho_abs =
          infer_expression state expr
        in
        (ty, (ty, ty') :: ty_eqs, rho_eqs, rho_ineqs, rho_abs)
    | Ast.Tuple exprs ->
        let fold expr (tys, ty_eqs, rho_eqs, rho_ineqs, rho_abs) =
          let ty', ty_eqs', rho_eqs', rho_ineqs', rho_abs' =
            infer_expression state expr
          in
          ( ty' :: tys,
            ty_eqs' @ ty_eqs,
            rho_eqs' @ rho_eqs,
            rho_ineqs' @ rho_ineqs,
            rho_abs' @ rho_abs )
        in
        let tys, ty_eqs, rho_eqs, rho_ineqs, rho_abs =
          List.fold_right fold exprs ([], [], [], [], [])
        in
        (Ast.TyTuple tys, ty_eqs, rho_eqs, rho_ineqs, rho_abs)
    | Ast.Lambda abs ->
        let ty, ty', ty_eqs, rho_eqs, rho_ineqs, rho_abs =
          infer_abstraction state abs
        in
        (Ast.TyArrow (ty, ty'), ty_eqs, rho_eqs, rho_ineqs, rho_abs)
    | Ast.PureLambda abs ->
        let ty, Ast.CompTy (ty', rho), ty_eqs, rho_eqs, rho_ineqs, rho_abs =
          infer_abstraction state abs
        in
        ( Ast.TyArrow (ty, CompTy (ty', rho)),
          ty_eqs,
          (rho, Ast.RhoConst ResourceGrade.zero) :: rho_eqs,
          rho_ineqs,
          rho_abs )
    | Ast.RecLambda (f, abs) ->
        let f_ty = fresh_ty () in
        let state' = extend_local_variables state [ (f, f_ty) ] in
        let ty, CompTy (ty', rho), ty_eqs, rho_eqs, rho_ineqs, rho_abs =
          infer_abstraction state' abs
        in
        let out_ty = Ast.TyArrow (ty, CompTy (ty', rho)) in
        ( out_ty,
          (f_ty, out_ty) :: ty_eqs,
          (rho, Ast.RhoConst ResourceGrade.zero) :: rho_eqs,
          rho_ineqs,
          rho_abs )
    | Ast.Variant (lbl, expr) -> (
        let ty_in, ty_out = infer_variant state lbl in
        match (ty_in, expr) with
        | None, None -> (ty_out, [], [], [], [])
        | Some ty_in, Some expr ->
            let ty, ty_eqs, rho_eqs, rho_ineqs, rho_abs =
              infer_expression state expr
            in
            (ty_out, (ty_in, ty) :: ty_eqs, rho_eqs, rho_ineqs, rho_abs)
        | None, Some _ | Some _, None ->
            Error.typing "Variant optional argument mismatch")
    | Ast.Handler (ret_case, op_cases) ->
        let arg_rho = fresh_rho () in
        let state' = extend_resource_grade state arg_rho in
        let ( arg_ty,
              Ast.CompTy (ret_ty, ret_rho),
              ty_eqs,
              rho_eqs,
              rho_ineqs,
              rho_abs ) =
          infer_abstraction state' ret_case
        in
        let ty_eqs', rho_eqs', rho_ineqs', rho_abs' =
          Ast.OpNameMap.fold
            (fun op op_case (ty_eqs'', rho_eqs'', rho_ineqs'', rho_abs'') ->
              let op_sig = Ast.OpNameMap.find_opt op state.op_signatures in
              match op_sig with
              | None -> Error.typing "Case for an unknown operation."
              | Some (param_ty, arity_ty, op_rho) ->
                  let ( op_args_ty,
                        Ast.CompTy (op_case_ty, op_case_rho),
                        op_ty_eqs,
                        op_rho_eqs,
                        op_rho_ineqs,
                        op_rho_abs ) =
                    infer_abstraction state op_case
                  in
                  let rho = fresh_rho () in
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
                     [rho] (Agda's [coerce]), so that e.g. a clause performing
                     [Send] once realises the grade [{Send | Send; Send}]. *)
                  let op_rho_ineqs' =
                    Ineq (op_case_rho, Ast.RhoAdd (op_rho, rho)) :: op_rho_ineqs
                  in
                  let op_rho_abs' = rho :: op_rho_abs in
                  ( op_ty_eqs' @ ty_eqs'',
                    op_rho_eqs' @ rho_eqs'',
                    op_rho_ineqs' @ rho_ineqs'',
                    op_rho_abs' @ rho_abs'' ))
            op_cases ([], [], [], [])
        in
        ( Ast.TyHandler (CompTy (arg_ty, arg_rho), CompTy (ret_ty, ret_rho)),
          ty_eqs @ ty_eqs',
          rho_eqs @ rho_eqs',
          rho_ineqs @ rho_ineqs',
          rho_abs @ rho_abs' )

  (** Returns:
      + inferred computation type
      + equational constraints between types
      + equational constraints between rhos
      + inequational constraints between rhos
      + list of abstractly quantified rhos (in operation cases of effect
        handlers) *)
  and infer_computation state = function
    | Ast.Return expr ->
        let ty, ty_eqs, rho_eqs, rho_ineqs, rho_abs =
          infer_expression state expr
        in
        ( Ast.CompTy (ty, Ast.RhoConst ResourceGrade.zero),
          ty_eqs,
          rho_eqs,
          rho_ineqs,
          rho_abs )
    | Ast.Do (comp1, comp2) ->
        let CompTy (ty1, rho1), ty_eqs1, rho_eqs1, rho_ineqs1, rho_abs1 =
          infer_computation state comp1
        in
        let comp_rho = fresh_rho () in
        let state' = extend_resource_grade state comp_rho in
        let ( ty1',
              Ast.CompTy (ty2, rho2),
              ty_eqs2,
              rho_eqs2,
              rho_ineqs2,
              rho_abs2 ) =
          infer_abstraction state' comp2
        in
        ( CompTy (ty2, Ast.RhoAdd (comp_rho, rho2)),
          ((ty1, ty1') :: ty_eqs1) @ ty_eqs2,
          ((rho1, comp_rho) :: rho_eqs1) @ rho_eqs2,
          rho_ineqs1 @ rho_ineqs2,
          rho_abs1 @ rho_abs2 )
    | Ast.Apply (e1, e2) ->
        let t1, ty_eqs1, rho_eqs1, rho_ineqs1, rho_abs1 =
          infer_expression state e1
        and t2, ty_eqs2, rho_eqs2, rho_ineqs2, rho_abs2 =
          infer_expression state e2
        and a = fresh_comp_ty () in
        ( a,
          ((t1, Ast.TyArrow (t2, a)) :: ty_eqs1) @ ty_eqs2,
          rho_eqs1 @ rho_eqs2,
          rho_ineqs1 @ rho_ineqs2,
          rho_abs1 @ rho_abs2 )
    | Ast.Match (e, cases) ->
        let ty1, ty_eqs, rho_eqs, rho_ineqs, rho_abs = infer_expression state e
        and branch_comp_ty = fresh_comp_ty () in
        let (CompTy (branch_ty, branch_rho)) = branch_comp_ty in
        let fold (ty_eqs, rho_eqs, rho_ineqs, rho_abs) abs =
          let ( ty1',
                CompTy (branch_ty', branch_rho'),
                ty_eqs',
                rho_eqs',
                rho_ineqs',
                rho_abs' ) =
            infer_abstraction state abs
          in
          ( ((ty1, ty1') :: (branch_ty, branch_ty') :: ty_eqs') @ ty_eqs,
            ((branch_rho, branch_rho') :: rho_eqs') @ rho_eqs,
            rho_ineqs' @ rho_ineqs,
            rho_abs' @ rho_abs )
        in
        let ty_eqs'', rho_eqs'', rho_ineqs'', rho_abs'' =
          List.fold_left fold (ty_eqs, rho_eqs, rho_ineqs, rho_abs) cases
        in
        (branch_comp_ty, ty_eqs'', rho_eqs'', rho_ineqs'', rho_abs'')
    | Ast.Delay (n, c) ->
        let rho = Ast.RhoConst (ResourceGrade.of_nat n) in
        let state' = extend_resource_grade state rho in
        let CompTy (ty, rho'), ty_eqs, rho_eqs, rho_ineqs, rho_abs =
          infer_computation state' c
        in
        ( CompTy (ty, Ast.RhoAdd (rho, rho')),
          ty_eqs,
          rho_eqs,
          rho_ineqs,
          rho_abs )
    | Ast.Box (rho, e, abs) ->
        let state_ahead = extend_resource_grade state rho in
        let value_ty, ty_eqs, rho_eqs, rho_ineqs, rho_abs =
          infer_expression state_ahead e
        in
        let value_ty', comp_ty, ty_eqs', rho_eqs', rho_ineqs', rho_abs' =
          infer_abstraction state abs
        in
        ( comp_ty,
          ((Ast.TyBox (rho, value_ty), value_ty') :: ty_eqs) @ ty_eqs',
          rho_eqs @ rho_eqs',
          rho_ineqs @ rho_ineqs',
          rho_abs @ rho_abs' )
    | Ast.Unbox (e, abs) ->
        let rec findVar e =
          match e with
          | Ast.Var x -> x
          | Ast.Annotated (e', _) -> findVar e'
          | _ -> Error.typing "Unboxing requires a variable."
        in
        let x = findVar e in
        let ty_params, rho_params, ty, _var_type =
          ContextHolderModule.find_variable x state.variables
        in
        let ty_subst = refreshing_ty_subst ty_params in
        let rho_subst = refreshing_rho_subst rho_params in
        let boxed_ty = Ast.substitute_ty ty_subst rho_subst ty in
        let value_ty, comp_ty, ty_eqs, rho_eqs, rho_ineqs, rho_abs =
          infer_abstraction state abs
        in
        let sum_rhos_added_after =
          ContextHolderModule.sum_rhos_added_after x state.variables
        in
        let rho = fresh_rho () in
        ( comp_ty,
          [ (Ast.TyBox (rho, value_ty), boxed_ty) ] @ ty_eqs,
          rho_eqs,
          [ Ineq (sum_rhos_added_after, rho) ] @ rho_ineqs,
          rho_abs )
    | Ast.Perform (op, e, abs) -> (
        let op_sig = Ast.OpNameMap.find_opt op state.op_signatures in
        match op_sig with
        | None -> Error.typing "Unknown operation call."
        | Some (param_ty, arity_ty, op_rho) ->
            let value_ty, ty_eqs, rho_eqs, rho_ineqs, rho_abs =
              infer_expression state e
            in
            let state_ahead = extend_resource_grade state op_rho in
            let ( value_ty',
                  CompTy (cont_ty, cont_rho),
                  ty_eqs',
                  rho_eqs',
                  rho_ineqs',
                  rho_abs' ) =
              infer_abstraction state_ahead abs
            in
            ( CompTy (cont_ty, Ast.RhoAdd (op_rho, cont_rho)),
              ((value_ty, param_ty) :: (value_ty', arity_ty) :: ty_eqs)
              @ ty_eqs',
              rho_eqs @ rho_eqs',
              rho_ineqs @ rho_ineqs',
              rho_abs @ rho_abs' ))
    | Ast.Handle (c, h) ->
        let CompTy (ty, rho), ty_eqs, rho_eqs, rho_ineqs, rho_abs =
          infer_computation state c
        in
        let ty', ty_eqs', rho_eqs', rho_ineqs', rho_abs' =
          infer_expression state h
        in
        let ty'' = fresh_ty () in
        let rho'' = fresh_rho () in
        ( CompTy (ty'', Ast.RhoAdd (rho, rho'')),
          (ty', Ast.TyHandler (CompTy (ty, rho), CompTy (ty'', rho'')))
          :: ty_eqs
          @ ty_eqs',
          rho_eqs @ rho_eqs',
          rho_ineqs @ rho_ineqs',
          rho_abs @ rho_abs' )

  and infer_abstraction state (pat, comp) =
    let ty, vars, ty_eqs = infer_pattern state pat in
    let state' = extend_local_variables state vars in
    let ty', ty_eqs', rho_eqs', rho_ineqs', rho_abs' =
      infer_computation state' comp
    in
    (ty, ty', ty_eqs @ ty_eqs', rho_eqs', rho_ineqs', rho_abs')

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
    let subst_inequation = function
      | Ineq (rho1, rho2) ->
          Ineq
            ( Ast.substitute_rho rho_subst rho1,
              Ast.substitute_rho rho_subst rho2 )
      | EternalOrIneq (ty, rho1, rho2) ->
          EternalOrIneq
            ( Ast.substitute_ty ty_subst rho_subst ty,
              Ast.substitute_rho rho_subst rho1,
              Ast.substitute_rho rho_subst rho2 )
    in
    List.map subst_inequation

  let subst_rho_abstract_constraints rho_subst =
    let subst_abstract_constraint = function
      | rho -> Ast.substitute_rho rho_subst rho
    in
    List.map subst_abstract_constraint

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
    | Ast.RhoConst _ -> false
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
    | Either.Left p1, Either.Left p2 -> compare p1 p2 (* compare parameters *)
    | Either.Right c1, Either.Right c2 -> compare c1 c2 (* compare constants *)
    | Either.Left _, _ -> -1
    | _, Either.Left _ -> 1

  (** [build_rho_param_list rho] lists the summands of [rho] from left to right,
      parameters as [Either.Left] and constants as [Either.Right]. *)
  let build_rho_param_list rho =
    let rec aux acc rho =
      match rho with
      | Ast.RhoParam t -> Either.Left t :: acc
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
      | Either.Left x -> Ast.RhoParam x
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
        | u1, u2 ->
            unify_rho_constraints state prev_unsolved_size
              ((u1, u2) :: unsolved) eqs)

  let rec unify_rho_ineq_constraints state prev_unsolved_size unsolved =
    let process wrap rho1 rho2 ineqs =
      let rho1' = simplify_rho rho1 in
      let rho2' = simplify_rho rho2 in
      match (rho1', rho2') with
      | _ when rho1' = rho2' ->
          unify_rho_ineq_constraints state prev_unsolved_size unsolved ineqs
      (* [ρ ≾ 0] with [ρ] still unknown: when the unit is the top of the
         sub-grade order the constraint holds outright and is discharged by
         [check_ineq], so leave it alone; otherwise take [ρ := 0], which
         satisfies the constraint by reflexivity. That is the unique solution
         when the unit is minimal, and a sound — if possibly incomplete —
         default when it is neither minimal nor top, as for the two-sided
         timed-trace grade. *)
      | Ast.RhoParam tp, rho
        when (not (occurs_rho tp rho))
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
    | EternalOrIneq (ty, rho1, rho2) :: ineqs ->
        process (fun r1 r2 -> EternalOrIneq (ty, r1, r2)) rho1 rho2 ineqs
    | Ineq (rho1, rho2) :: ineqs ->
        process (fun r1 r2 -> Ineq (r1, r2)) rho1 rho2 ineqs

  let is_eternal state ty =
    (* [visited] tracks type names currently being unfolded to prevent infinite
       recursion on recursive types (e.g. list). When a type name is encountered
       again in [visited], we return true as a coinductive assumption: the
       recursive occurrence is guarded and the non-recursive parts were already
       checked in the outer call. *)
    let rec check visited = function
      | Ast.TyConst c -> Const.is_eternal_ty c
      | Ast.TyApply (ty_name, args) -> (
          List.for_all (check visited) args
          &&
          if List.mem ty_name visited then true
          else
            let visited' = ty_name :: visited in
            match Ast.TyNameMap.find_opt ty_name state.type_definitions with
            | None -> false
            | Some (params, Ast.TyInline ty) ->
                let ty_subst =
                  List.fold_left2
                    (fun subst param arg -> Ast.TyParamMap.add param arg subst)
                    Ast.TyParamMap.empty params args
                in
                check visited'
                  (Ast.substitute_ty ty_subst Ast.RhoParamMap.empty ty)
            | Some (params, Ast.TySum variants) ->
                let ty_subst =
                  List.fold_left2
                    (fun subst param arg -> Ast.TyParamMap.add param arg subst)
                    Ast.TyParamMap.empty params args
                in
                List.for_all
                  (fun (_, arg_ty) ->
                    match arg_ty with
                    | None -> true
                    | Some ty ->
                        check visited'
                          (Ast.substitute_ty ty_subst Ast.RhoParamMap.empty ty))
                  variants)
      | Ast.TyParam _ -> false
      | Ast.TyArrow _ -> false
      | Ast.TyTuple tys -> List.for_all (check visited) tys
      | Ast.TyBox _ -> false
      | Ast.TyHandler _ -> false
    in
    check [] ty

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

  let rec check_rho_ineq_constraints state =
    let check_ineq ?eternal_ty rho_smaller rho_greater_or_equal =
      let rho_smaller_simplified = simplify_rho rho_smaller in
      let rho_greater_or_equal_simplified = simplify_rho rho_greater_or_equal in
      let rho_pp = PrettyPrint.RhoPrintParam.create () in
      let print_rho rho ppf =
        PrettyPrint.print_rho (module ResourceGrade) rho_pp rho ppf
      in
      try
        let rho_greater_or_equal_val =
          ContextHolderModule.eval_rho rho_greater_or_equal_simplified
        in
        if
          rho_greater_or_equal_val = ResourceGrade.zero
          && ResourceGrade.is_zero_top_sub_rho
        then ()
        else
          let rho_smaller_val =
            ContextHolderModule.eval_rho rho_smaller_simplified
          in
          if
            not
              (ResourceGrade.is_sub_rho (op_bounds state) rho_smaller_val
                 rho_greater_or_equal_val)
          then
            raise
              (Exception.InequalityCheckFailed
                 "ResourceGrade inequality check failed")
      with
      | Exception.InequalityCheckFailed _ -> (
          match eternal_ty with
          | Some ty when is_eternal state ty -> ()
          | Some ty ->
              let ty_pp = PrettyPrint.TyPrintParam.create () in
              Error.typing
                "Type %t is not eternal and resource inequality %t %s %t failed"
                (PrettyPrint.print_ty (module ResourceGrade) ty_pp rho_pp ty)
                (print_rho rho_smaller_simplified)
                ResourceGrade.is_sub_rho_symbol
                (print_rho rho_greater_or_equal_simplified)
          | None ->
              Error.typing "Comparing resource inequality %t %s %t failed"
                (print_rho rho_smaller_simplified)
                ResourceGrade.is_sub_rho_symbol
                (print_rho rho_greater_or_equal_simplified))
      | Exception.RhoParamInEval _ -> (
          match eternal_ty with
          | Some ty when is_eternal state ty -> ()
          | Some ty ->
              let ty_pp = PrettyPrint.TyPrintParam.create () in
              Error.typing
                "Type %t is not eternal and cannot compare non-ground resource \
                 values %t and %t"
                (PrettyPrint.print_ty (module ResourceGrade) ty_pp rho_pp ty)
                (print_rho rho_smaller_simplified)
                (print_rho rho_greater_or_equal_simplified)
          | None ->
              Error.typing "Cannot compare non-ground resource values %t and %t"
                (print_rho rho_smaller_simplified)
                (print_rho rho_greater_or_equal_simplified))
    in
    function
    | [] -> ()
    | EternalOrIneq (ty, rho_smaller, rho_greater_or_equal) :: rho_ineqs ->
        check_ineq ~eternal_ty:ty rho_smaller rho_greater_or_equal;
        check_rho_ineq_constraints state rho_ineqs
    | Ineq (rho_smaller, rho_greater_or_equal) :: rho_ineqs ->
        check_ineq rho_smaller rho_greater_or_equal;
        check_rho_ineq_constraints state rho_ineqs

  let rec check_rho_abs_constraints = function
    | [] -> ()
    | rho :: rho_abs -> (
        match rho with
        | Ast.RhoParam _ -> check_rho_abs_constraints rho_abs
        | _ ->
            Error.typing "ResourceGrade grade is not in abstract form %t"
              (fun ppf ->
                PrettyPrint.print_rho
                  (module ResourceGrade)
                  (PrettyPrint.RhoPrintParam.create ())
                  rho ppf))

  let unify state ty_eqs rho_eqs rho_ineqs rho_abs =
    (* let ty_pp = PrettyPrint.TyPrintParam.create () in
    let rho_pp = PrettyPrint.RhoPrintParam.create () in
    print_ty_constraints_pp ty_pp rho_pp ty_eqs;
    print_rho_eq_constraints_pp rho_pp rho_eqs;
    print_rho_ineq_constraints_pp rho_pp rho_ineqs; *)
    let ty_subst, rho_eqs' = unify_ty_constraints state [] ty_eqs in
    (* print_rho_eq_constraints_pp rho_pp rho_eqs'; *)
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
    (* print_rho_ineq_constraints_pp rho_pp
      (subst_rho_inequations ty_subst rho_subst'' rho_ineqs'')); *)
    check_rho_ineq_constraints state
      (subst_rho_inequations ty_subst rho_subst'' rho_ineqs'');
    check_rho_abs_constraints
      (subst_rho_abstract_constraints rho_subst'' rho_abs);
    let ty_subst' =
      Ast.TyParamMap.map
        (fun ty -> Ast.substitute_ty ty_subst rho_subst'' ty)
        ty_subst
    in
    (ty_subst', rho_subst'')

  let infer state e =
    let comp_ty, ty_eqs, rho_eqs, rho_ineqs, rho_abs =
      infer_computation state e
    in
    let ty_subst, rho_subst = unify state ty_eqs rho_eqs rho_ineqs rho_abs in
    let comp_ty' = Ast.substitute_comp_ty ty_subst rho_subst comp_ty in
    simplify_comp_ty comp_ty'

  let add_external_function x ty_sch state =
    {
      state with
      variables = ContextHolderModule.add_variable x ty_sch state.variables;
    }

  let add_top_definition state x e =
    (* Format.fprintf Format.std_formatter "\n";
    PrettyPrint.print_expression (module ResourceGrade) e Format.std_formatter;
    Format.fprintf Format.std_formatter "\n"; *)
    let ty, ty_eqs, rho_eqs, rho_ineqs, rho_abs = infer_expression state e in
    let ty_subst, rho_subst = unify state ty_eqs rho_eqs rho_ineqs rho_abs in
    let ty' = Ast.substitute_ty ty_subst rho_subst ty in
    let ty'' = simplify_ty ty' in
    let free_vars, free_rhos = Ast.free_vars ty'' in
    let ty_sch =
      ( free_vars |> Ast.TyParamSet.elements,
        free_rhos |> Ast.RhoParamSet.elements,
        ty'',
        Global )
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

  (* The runtime bounds an operation declares must agree with the runs its grade
     promises: [lo] may not undercut the fastest run the grade allows and [hi]
     must cover the slowest one, where a run costs its delays plus, for each of
     its events, the matching end of the bounds of the operation named. The
     operation's own bounds are used for its own events, so an operation graded
     by the single run that is itself is trivially consistent, while a
     self-referential grade such as [{Send | Send; Send}] never is, its retry
     run costing twice the upper bound. This is the duration morphism of the
     Agda [System/Traces/Timed/*] modules, [minSetDuration] for the lower
     bound and [setDuration] for the upper bound. *)
  let check_op_bounds op_name (lo, hi) bounds grade =
    match ResourceGrade.implied_bounds bounds grade with
    | Some (imp_lo, imp_hi) when lo > imp_lo || hi < imp_hi ->
        Error.typing
          "the runtime bounds of operation %s, within (%d, %d), are \
           inconsistent with its grade %s, whose runs take between %d and %d \
           time units"
          op_name lo hi (ResourceGrade.show grade) imp_lo imp_hi
    | Some _ | None -> ()

  let add_operation_signature state (op, ty1, ty2, rho, bounds) =
    let op_name = Ast.OpName.string_of op in
    let op_bounds' =
      match (ResourceGrade.needs_op_bounds, bounds) with
      | true, None ->
          Error.typing
            "operation %s needs runtime bounds `within (lo, hi)` under the \
             '%s' grading monoid"
            op_name ResourceGrade.name
      | false, Some _ ->
          Error.typing
            "runtime bounds are only used by the timed-trace grading monoids; \
             under '%s' the operation grade already carries them"
            ResourceGrade.name
      | false, None -> state.op_bounds
      | true, Some (lo, hi) ->
          if lo > hi then
            Error.typing
              "the runtime bounds of operation %s must satisfy lo <= hi" op_name
          else if hi < 1 then
            Error.typing
              "the upper runtime bound of operation %s must be at least 1"
              op_name
          else StringMap.add op_name (lo, hi) state.op_bounds
    in
    (* The grade of an operation is always a literal, so its events can be
       validated right away; the operation may name itself, as in
       [Send : unit ~> unit # {Send | Send; Send}]. *)
    (match rho with
    | Ast.RhoConst grade ->
        List.iter
          (fun ev ->
            if not (StringMap.mem ev op_bounds') then
              Error.typing "unknown event '%s' in the grade of operation %s" ev
                op_name)
          (ResourceGrade.events grade)
    | Ast.RhoParam _ | Ast.RhoAdd _ -> ());
    (* every event of the grade is now known to be declared, so the lookup in
       [op_bounds'] --- which already holds the bounds of the operation being
       declared --- is total *)
    (match (rho, bounds) with
    | Ast.RhoConst grade, Some declared ->
        check_op_bounds op_name declared
          (fun ev -> StringMap.find ev op_bounds')
          grade
    | (Ast.RhoConst _ | Ast.RhoParam _ | Ast.RhoAdd _), _ -> ());
    {
      state with
      op_signatures = Ast.OpNameMap.add op (ty1, ty2, rho) state.op_signatures;
      op_bounds = op_bounds';
    }

  let load_primitive state x prim =
    let ty_params, rho_params, ty = P.primitive_type_scheme prim in
    add_external_function x (ty_params, rho_params, ty, Global) state
end
