module Error = Utils.Error
module Location = Utils.Location
module StringMap = Utils.StringMap
module Ast = Language.Ast
module Const = Language.Const
module Context = Language.Context
module Exception = Language.Exception
module PrettyPrint = Language.PrettyPrint

module Make (ResourceGrade : Language.ResourceGrade.Grade) = struct
  module E = Explain.Make (ResourceGrade)

  (* A grade accumulated in the context, with where and how it was spent, so
     that a message about a variable that may no longer be used can point at
     each [delay], [perform] and sequenced computation in between. *)
  module Elapsed = struct
    type t = {
      rho : ResourceGrade.t Ast.rho;
      at : Location.t;
      kind : Ast.elapsed_kind;
    }

    let rho e = e.rho
  end

  module ContextHolderModule =
    Context.Make (Ast.Variable) (Map.Make (Ast.Variable)) (ResourceGrade)
      (Elapsed)

  module P = Primitives.Make (ResourceGrade)

  type ty = ResourceGrade.t Ast.ty
  type rho = ResourceGrade.t Ast.rho
  type comp_ty = ResourceGrade.t Ast.comp_ty
  type constr = ResourceGrade.t Ast.constr

  type reason = ResourceGrade.t Ast.reason
  (** The reasons a constraint may carry, at the grades this run is checking:
      the elapsed entries of a reason are grades like any other. *)

  type var_type = Global | Local

  type entry = {
    scheme : ResourceGrade.t Ast.ty_scheme;
    scope : var_type;
    bound_at : Location.t option;
        (** Where the variable was bound: the pattern for a local, the command
            for a top-level definition. A primitive has none. *)
  }

  let scheme_of entry = entry.scheme

  type state = {
    variables : entry ContextHolderModule.t;
        (** Local variables get a monomorphic, unqualified scheme. *)
    type_definitions :
      (Ast.ty_param list * ResourceGrade.t Ast.ty_def) Ast.TyNameMap.t;
    noneternal_types : Ast.TyNameSet.t;
        (** The type names declared with [noneternal type ...]. Their values are
            never eternal, whatever their structure says, and so is anything
            built out of them. *)
    op_signatures : (ty * ty * rho * Location.t) Ast.OpNameMap.t;
        (** The signature of each operation, with the span of the [operation]
            command, which every message about an operation points back at. *)
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

  let rec check_ty ~loc state = function
    | Ast.TyConst _ -> ()
    | TyApply (ty_name, tys) ->
        let params, _ = Ast.TyNameMap.find ty_name state.type_definitions in
        let expected, actual = (List.length params, List.length tys) in
        if expected <> actual then
          Error.typing ~loc "Type %t expects %d argument%s but is given %d"
            (Ast.TyName.print ty_name) expected
            (if expected = 1 then "" else "s")
            actual
        else List.iter (check_ty ~loc state) tys
    | TyParam _ -> ()
    | TyArrow (ty1, ty2) ->
        check_ty ~loc state ty1;
        check_comp_ty ~loc state ty2
    | TyTuple tys -> List.iter (check_ty ~loc state) tys
    | TyBox (_, ty) -> check_ty ~loc state ty
    | TyHandler (ty1, ty2) ->
        check_comp_ty ~loc state ty1;
        check_comp_ty ~loc state ty2

  and check_comp_ty ~loc state = function
    | Ast.CompTy (ty, _rho) -> check_ty ~loc state ty

  let check_variant ~loc state (_label, arg_ty) =
    match arg_ty with None -> () | Some ty -> check_ty ~loc state ty

  let check_ty_def ~loc state = function
    | Ast.TySum defs -> List.iter (check_variant ~loc state) defs
    | Ast.TyInline ty -> check_ty ~loc state ty

  let fresh_ty () =
    let a = Ast.TyParamModule.fresh "ty" in
    Ast.TyParam a

  let fresh_rho () =
    let t = Ast.RhoParamModule.fresh "rho" in
    Ast.RhoParam t

  let fresh_comp_ty () = Ast.CompTy (fresh_ty (), fresh_rho ())

  (* ------------------------------------------------------------------ *)
  (* Constraints                                                         *)
  (* ------------------------------------------------------------------ *)

  type ty_eq = {
    lhs : ty;
    rhs : ty;
    reason : reason;
    root : ty * ty;
        (** The equation this one was decomposed out of, for the "while
            matching" note: a failing equation is usually a small part of it. *)
    via : Ast.ty_param option * Ast.ty_param option;
        (** Which side, if either, arrived by substituting a type parameter
            away; where that parameter was decided is the real disagreement. *)
  }

  (* The fields are prefixed: a grade equation and a type equation would
     otherwise share every label, and a record pattern would silently pick
     whichever type was declared last. *)
  type rho_eq = {
    grade_lhs : rho;
    grade_rhs : rho;
    grade_reason : reason;
    grade_root : (ty * ty) option;
        (** The type equation this one was decomposed out of, if any: the grades
            of a function type are compared because the types are. *)
  }

  type constraints = {
    ty_eqs : ty_eq list;
    rho_eqs : rho_eq list;
    ineqs : constr list;
  }
  (** Everything one subterm asks of the solver. A monoid, so that inference
      reads as "the constraints of the parts, and these". *)

  let empty = { ty_eqs = []; rho_eqs = []; ineqs = [] }

  let union c1 c2 =
    {
      ty_eqs = c1.ty_eqs @ c2.ty_eqs;
      rho_eqs = c1.rho_eqs @ c2.rho_eqs;
      ineqs = c1.ineqs @ c2.ineqs;
    }

  let concat cs = List.fold_right union cs empty

  (* A reason for a constraint a construct generates directly, undecomposed. *)
  let because at why : reason = { at; why; path = [] }

  (* A syntax node and a reason both have an [at]; these say which one is meant
     where the surrounding code cannot. *)
  let reason_at (r : reason) = r.Ast.at
  let reason_path (r : reason) = r.Ast.path

  let ty_eq reason lhs rhs =
    {
      empty with
      ty_eqs = [ { lhs; rhs; reason; root = (lhs, rhs); via = (None, None) } ];
    }

  let rho_eq reason lhs rhs =
    {
      empty with
      rho_eqs =
        [
          {
            grade_lhs = lhs;
            grade_rhs = rhs;
            grade_reason = reason;
            grade_root = None;
          };
        ];
    }

  let ineq reason rho1 rho2 =
    { empty with ineqs = [ Ast.Ineq (rho1, rho2, reason) ] }

  let constrs cs = { empty with ineqs = cs }

  let constr_reason : constr -> reason = function
    | Ast.Ineq (_, _, reason)
    | Ast.Eternal (_, reason)
    | Ast.EternalOrIneq (_, _, _, reason) ->
        reason

  (* ------------------------------------------------------------------ *)
  (* The context                                                         *)
  (* ------------------------------------------------------------------ *)

  let monomorphic ty : ResourceGrade.t Ast.ty_scheme =
    { ty_params = []; rho_params = []; constrs = []; ty }

  let extend_local_variables state vars =
    List.fold_left
      (fun state (x, ty, at) ->
        let entry =
          { scheme = monomorphic ty; scope = Local; bound_at = Some at }
        in
        {
          state with
          variables = ContextHolderModule.add_variable x entry state.variables;
        })
      state vars

  let extend_resource_grade state rho ~at ~kind =
    let updated_variables =
      ContextHolderModule.add_temp { Elapsed.rho; at; kind } state.variables
    in
    { state with variables = updated_variables }

  (* The grades accumulated since [x] was bound, oldest first, each with where
     it was spent: a label is only printed once its grade is known not to be
     zero, and here most of them are still unsolved parameters. *)
  let elapsed_after x state =
    List.map
      (fun (e : Elapsed.t) -> (e.Elapsed.rho, e.Elapsed.at, e.Elapsed.kind))
      (ContextHolderModule.elapsed_after x state.variables)

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

  (** The qualifier of a scheme, instantiated at a use of [x]. The definition's
      own reason is kept as the [inner] reason, so a message can say why the
      definition needs the constraint and not just that this use inherits it. *)
  let instantiate_constrs ty_subst rho_subst ~var ~defined_at ~use_at cs =
    List.map
      (fun c ->
        Ast.substitute_constr ty_subst rho_subst
          (Ast.wrap_reason
             (fun inner ->
               because use_at (Ast.InstanceOf { var; defined_at; inner }))
             c))
      cs

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

  (* An operation case, or any other abstraction, spans its pattern and its
     body: neither alone is the construct a message should point at. *)
  let abstraction_span ((pat, comp) : _ Ast.abstraction) =
    Location.merge pat.Ast.at comp.Ast.at

  (** How to describe the grade a [Do] puts into the context. The desugarer
      turns [delay n; c] and [perform Op e; c] into [Do]s whose bound
      computation is exactly that grade, so those two can be named outright;
      with a body of its own only part of the grade is theirs, and the message
      falls back to the computation itself. *)
  let sequenced_kind (comp : _ Ast.computation) =
    match comp.it with
    | Ast.Delay (n, { it = Ast.Return _; _ }) -> Ast.Delayed n
    | Ast.Perform (op, _, (_, { it = Ast.Return _; _ })) -> Ast.Performed op
    | _ -> Ast.Sequenced

  (** Each variable the pattern binds comes with the span of its binder. *)
  let rec infer_pattern state (pat : _ Ast.pattern) =
    match pat.it with
    | Ast.PVar x ->
        let ty = fresh_ty () in
        (ty, [ (x, ty, pat.at) ], empty)
    | Ast.PAs (pat', x) ->
        let ty, vars, cs = infer_pattern state pat' in
        (ty, (x, ty, pat.at) :: vars, cs)
    | Ast.PAnnotated (pat', ty) ->
        let ty', vars, cs = infer_pattern state pat' in
        ( ty,
          vars,
          union (ty_eq (because pat.at Ast.PatternAnnotation) ty ty') cs )
    | Ast.PConst c -> (Ast.TyConst (Const.infer_ty c), [], empty)
    | Ast.PNonbinding ->
        let ty = fresh_ty () in
        (ty, [], empty)
    | Ast.PTuple pats ->
        let fold pat' (tys, vars, cs) =
          let ty', vars', cs' = infer_pattern state pat' in
          (ty' :: tys, vars' @ vars, union cs' cs)
        in
        let tys, vars, cs = List.fold_right fold pats ([], [], empty) in
        (Ast.TyTuple tys, vars, cs)
    | Ast.PVariant (lbl, arg) -> (
        let ty_in, ty_out = infer_variant state lbl in
        match (ty_in, arg) with
        | None, None -> (ty_out, [], empty)
        | Some ty_in, Some arg ->
            let ty, vars, cs = infer_pattern state arg in
            ( ty_out,
              vars,
              union
                (ty_eq (because arg.Ast.at (Ast.VariantArgument lbl)) ty_in ty)
                cs )
        | None, Some _ ->
            Error.typing ~loc:pat.at
              "Constructor %s takes no argument but is given one"
              (Ast.Label.string_of lbl)
        | Some _, None ->
            Error.typing ~loc:pat.at
              "Constructor %s takes an argument but is given none"
              (Ast.Label.string_of lbl))

  and infer_expression state (e : _ Ast.expression) =
    match e.it with
    | Ast.Var x ->
        let { scheme; scope; bound_at } =
          ContextHolderModule.find_variable x state.variables
        in
        let ty = scheme.Ast.ty in
        let use_constrs =
          match scope with
          | Local ->
              (* A local variable may be used only if its type is eternal or
                 nothing has elapsed since it was bound. *)
              let why =
                Ast.UseAfterTime
                  {
                    var = x;
                    bound_at = Option.value bound_at ~default:e.at;
                    elapsed = elapsed_after x state;
                  }
              in
              [
                Ast.EternalOrIneq
                  ( ty,
                    ContextHolderModule.sum_rhos_added_after x state.variables,
                    Ast.RhoConst ResourceGrade.zero,
                    because e.at why );
              ]
          | Global -> []
        in
        let ty_subst = refreshing_ty_subst scheme.Ast.ty_params in
        let rho_subst = refreshing_rho_subst scheme.Ast.rho_params in
        let instance =
          instantiate_constrs ty_subst rho_subst ~var:x ~defined_at:bound_at
            ~use_at:e.at scheme.Ast.constrs
        in
        ( Ast.substitute_ty ty_subst rho_subst ty,
          constrs (use_constrs @ instance) )
    | Ast.Const c -> (Ast.TyConst (Const.infer_ty c), empty)
    | Ast.Annotated (expr, ty) -> (
        let ty', cs = infer_expression state expr in
        let r = because e.at Ast.Annotation in
        match (ty, ty') with
        | ( Ast.TyArrow (arg_ty, CompTy (res_ty, rho)),
            Ast.TyArrow (arg_ty', CompTy (res_ty', rho')) ) ->
            (* An annotation on a function is a sub-effecting coercion, as an
               operation case or a default implementation is: the grade the
               body accumulates need only be a sub-grade of the stated one,
               while the argument and result types must agree exactly. *)
            ( ty,
              concat
                [
                  ty_eq r arg_ty arg_ty';
                  ty_eq r res_ty res_ty';
                  cs;
                  ineq r rho' rho;
                ] )
        | _ -> (ty, union (ty_eq r ty ty') cs))
    | Ast.Tuple exprs ->
        let fold expr (tys, cs) =
          let ty', cs' = infer_expression state expr in
          (ty' :: tys, union cs' cs)
        in
        let tys, cs = List.fold_right fold exprs ([], empty) in
        (Ast.TyTuple tys, cs)
    | Ast.Lambda abs ->
        let ty, ty', cs = infer_abstraction state abs in
        (Ast.TyArrow (ty, ty'), cs)
    | Ast.PureLambda abs ->
        let ty, Ast.CompTy (ty', rho), cs = infer_abstraction state abs in
        ( Ast.TyArrow (ty, CompTy (ty', rho)),
          union cs
            (rho_eq
               (because e.at Ast.PureBody)
               rho (Ast.RhoConst ResourceGrade.zero)) )
    | Ast.RecLambda (f, abs) ->
        let f_ty = fresh_ty () in
        let state' = extend_local_variables state [ (f, f_ty, e.at) ] in
        let ty, CompTy (ty', rho), cs = infer_abstraction state' abs in
        let out_ty = Ast.TyArrow (ty, CompTy (ty', rho)) in
        ( out_ty,
          concat
            [
              ty_eq (because e.at (Ast.RecursiveDefinition f)) f_ty out_ty;
              cs;
              rho_eq
                (because e.at Ast.PureBody)
                rho (Ast.RhoConst ResourceGrade.zero);
            ] )
    | Ast.Variant (lbl, arg) -> (
        let ty_in, ty_out = infer_variant state lbl in
        match (ty_in, arg) with
        | None, None -> (ty_out, empty)
        | Some ty_in, Some arg ->
            let ty, cs = infer_expression state arg in
            ( ty_out,
              union
                (ty_eq (because arg.Ast.at (Ast.VariantArgument lbl)) ty_in ty)
                cs )
        | None, Some _ ->
            Error.typing ~loc:e.at
              "Constructor %s takes no argument but is given one"
              (Ast.Label.string_of lbl)
        | Some _, None ->
            Error.typing ~loc:e.at
              "Constructor %s takes an argument but is given none"
              (Ast.Label.string_of lbl))
    | Ast.Handler (ret_case, op_cases) ->
        let arg_rho = fresh_rho () in
        (* The return clause runs once the handled computation has, so it is
           checked that much further along. *)
        let state' =
          extend_resource_grade state arg_rho ~at:e.at ~kind:Ast.Handled
        in
        let arg_ty, Ast.CompTy (ret_ty, ret_rho), cs =
          infer_abstraction state' ret_case
        in
        let cs' =
          Ast.OpNameMap.fold
            (fun op op_case acc ->
              let case_at = abstraction_span op_case in
              match Ast.OpNameMap.find_opt op state.op_signatures with
              | None ->
                  Error.typing ~loc:case_at "Case for an unknown operation %s"
                    (Ast.OpName.string_of op)
              | Some (param_ty, arity_ty, op_rho, signature_at) ->
                  let op_args_ty, Ast.CompTy (op_case_ty, op_case_rho), case_cs
                      =
                    infer_abstraction state op_case
                  in
                  (* The case must be well-typed for every grade of the
                     continuation, so that grade is rigid. *)
                  let rho = Ast.RhoRigid (Ast.RhoParamModule.fresh "rho") in
                  let r =
                    because case_at (Ast.HandlerCase { op; signature_at })
                  in
                  let r' =
                    because case_at (Ast.ContinuationGrade { op; signature_at })
                  in
                  concat
                    [
                      ty_eq r op_case_ty ret_ty;
                      ty_eq r op_args_ty
                        (Ast.TyTuple
                           [
                             param_ty;
                             Ast.TyBox
                               ( op_rho,
                                 Ast.TyArrow (arity_ty, CompTy (ret_ty, rho)) );
                           ]);
                      case_cs;
                      (* The clause need only be a sub-effect of the
                         operation's declared grade extended by the
                         continuation's grade [rho] (sub-effecting), so that
                         e.g. a clause performing [Send] once realises the
                         grade [{Send | Send; Send}]. *)
                      ineq r' op_case_rho (Ast.RhoAdd (op_rho, rho));
                      acc;
                    ])
            op_cases empty
        in
        ( Ast.TyHandler (CompTy (arg_ty, arg_rho), CompTy (ret_ty, ret_rho)),
          union cs cs' )

  and infer_computation state (c : _ Ast.computation) =
    match c.it with
    | Ast.Return expr ->
        let ty, cs = infer_expression state expr in
        (Ast.CompTy (ty, Ast.RhoConst ResourceGrade.zero), cs)
    | Ast.Do (comp1, ((pat, _) as comp2)) ->
        let Ast.CompTy (ty1, rho1), cs1 = infer_computation state comp1 in
        let comp_rho = fresh_rho () in
        let state' =
          extend_resource_grade state comp_rho ~at:comp1.at
            ~kind:(sequenced_kind comp1)
        in
        let ty1', Ast.CompTy (ty2, rho2), cs2 =
          infer_abstraction state' comp2
        in
        ( CompTy (ty2, Ast.RhoAdd (comp_rho, rho2)),
          concat
            [
              (* A [let] binding is a one-case match: the bound computation is
                 the value the pattern is matched against. *)
              ty_eq
                (because pat.Ast.at
                   (Ast.MatchScrutinee { scrutinee_at = comp1.at }))
                ty1 ty1';
              rho_eq (because comp1.at Ast.Sequencing) rho1 comp_rho;
              cs1;
              cs2;
            ] )
    | Ast.Apply (e1, e2) ->
        let t1, cs1 = infer_expression state e1
        and t2, cs2 = infer_expression state e2
        and a = fresh_comp_ty () in
        ( a,
          concat
            [
              ty_eq
                (because c.at
                   (Ast.Application { func_at = e1.Ast.at; arg_at = e2.Ast.at }))
                t1
                (Ast.TyArrow (t2, a));
              cs1;
              cs2;
            ] )
    | Ast.Match (e, cases) ->
        let ty1, cs = infer_expression state e
        and branch_comp_ty = fresh_comp_ty () in
        let (CompTy (branch_ty, branch_rho)) = branch_comp_ty in
        let fold acc ((pat, body) as abs) =
          let ty1', CompTy (branch_ty', branch_rho'), cs' =
            infer_abstraction state abs
          in
          concat
            [
              ty_eq
                (because pat.Ast.at
                   (Ast.MatchScrutinee { scrutinee_at = e.Ast.at }))
                ty1 ty1';
              ty_eq (because body.Ast.at Ast.MatchBranch) branch_ty branch_ty';
              cs';
              rho_eq
                (because body.Ast.at Ast.MatchBranch)
                branch_rho branch_rho';
              acc;
            ]
        in
        (branch_comp_ty, List.fold_left fold cs cases)
    | Ast.Delay (n, c') ->
        let rho = Ast.RhoConst (ResourceGrade.of_nat n) in
        let state' =
          extend_resource_grade state rho ~at:c.at ~kind:(Ast.Delayed n)
        in
        let CompTy (ty, rho'), cs = infer_computation state' c' in
        (CompTy (ty, Ast.RhoAdd (rho, rho')), cs)
    | Ast.Box (rho, e, abs) ->
        let state_ahead =
          extend_resource_grade state rho ~at:c.at ~kind:Ast.Boxed
        in
        let value_ty, cs = infer_expression state_ahead e in
        let value_ty', comp_ty, cs' = infer_abstraction state abs in
        ( comp_ty,
          concat
            [
              ty_eq
                (because c.at Ast.BoxedValue)
                (Ast.TyBox (rho, value_ty))
                value_ty';
              cs;
              cs';
            ] )
    | Ast.Unbox (e, abs) ->
        let rec find_var (e' : _ Ast.expression) =
          match e'.Ast.it with
          | Ast.Var x -> x
          | Ast.Annotated (e'', _) -> find_var e''
          | _ -> Error.typing ~loc:e'.Ast.at "Only a variable can be unboxed"
        in
        let x = find_var e in
        let { scheme; scope = _; bound_at } =
          ContextHolderModule.find_variable x state.variables
        in
        let ty_subst = refreshing_ty_subst scheme.Ast.ty_params in
        let rho_subst = refreshing_rho_subst scheme.Ast.rho_params in
        let boxed_ty = Ast.substitute_ty ty_subst rho_subst scheme.Ast.ty in
        let instance =
          instantiate_constrs ty_subst rho_subst ~var:x ~defined_at:bound_at
            ~use_at:e.Ast.at scheme.Ast.constrs
        in
        let value_ty, comp_ty, cs = infer_abstraction state abs in
        let sum_rhos_added_after =
          ContextHolderModule.sum_rhos_added_after x state.variables
        in
        let rho = fresh_rho () in
        let r =
          because c.at
            (Ast.Unboxed { var = x; bound_at; elapsed = elapsed_after x state })
        in
        ( comp_ty,
          concat
            [
              ty_eq r (Ast.TyBox (rho, value_ty)) boxed_ty;
              cs;
              ineq r sum_rhos_added_after rho;
              constrs instance;
            ] )
    | Ast.Perform (op, e, ((pat, _) as abs)) -> (
        match Ast.OpNameMap.find_opt op state.op_signatures with
        | None ->
            Error.typing ~loc:c.at "Unknown operation %s"
              (Ast.OpName.string_of op)
        | Some (param_ty, arity_ty, op_rho, signature_at) ->
            let value_ty, cs = infer_expression state e in
            let state_ahead =
              extend_resource_grade state op_rho ~at:c.at
                ~kind:(Ast.Performed op)
            in
            let value_ty', CompTy (cont_ty, cont_rho), cs' =
              infer_abstraction state_ahead abs
            in
            ( CompTy (cont_ty, Ast.RhoAdd (op_rho, cont_rho)),
              concat
                [
                  ty_eq
                    (because e.Ast.at
                       (Ast.PerformArgument { op; signature_at }))
                    value_ty param_ty;
                  ty_eq
                    (because pat.Ast.at
                       (Ast.PerformContinuation { op; signature_at }))
                    value_ty' arity_ty;
                  cs;
                  cs';
                ] ))
    | Ast.Handle (c1, h) ->
        let CompTy (ty, rho), cs = infer_computation state c1 in
        let ty', cs' = infer_expression state h in
        let ty'' = fresh_ty () in
        let rho'' = fresh_rho () in
        ( CompTy (ty'', Ast.RhoAdd (rho, rho'')),
          concat
            [
              ty_eq
                (because h.Ast.at Ast.HandleWith)
                ty'
                (Ast.TyHandler (CompTy (ty, rho), CompTy (ty'', rho'')));
              cs;
              cs';
            ] )

  and infer_abstraction state (pat, comp) =
    let ty, vars, cs = infer_pattern state pat in
    let state' = extend_local_variables state vars in
    let ty', cs' = infer_computation state' comp in
    (ty, ty', union cs cs')

  (* ------------------------------------------------------------------ *)
  (* Substitutions with provenance                                       *)
  (* ------------------------------------------------------------------ *)

  (** A substitution that remembers, for each parameter it binds, the reason of
      the equation that decided it, so that a diagnostic can point at the place
      a variable was pinned down. {!add_ty} applies the substitution so far. *)
  module Subst : sig
    type t

    val empty : t
    val add_ty : Ast.ty_param -> ty -> reason -> t -> t
    val add_rho : Ast.rho_param -> rho -> reason -> t -> t
    val apply_ty : t -> ty -> ty
    val apply_rho : t -> rho -> rho
    val apply_comp_ty : t -> comp_ty -> comp_ty
    val apply_constr : t -> constr -> constr
    val ty_reason : Ast.ty_param -> t -> reason option
    val rho_reason : Ast.rho_param -> t -> reason option
    val find_ty : Ast.ty_param -> t -> ty option
    val map_ty : (ty -> ty) -> t -> t
    val map_rho : (rho -> rho) -> t -> t
    val union_prefer_right : t -> t -> t
  end = struct
    type t = {
      tys : ty Ast.TyParamMap.t;
      rhos : rho Ast.RhoParamMap.t;
      ty_reasons : reason Ast.TyParamMap.t;
      rho_reasons : reason Ast.RhoParamMap.t;
    }

    let empty =
      {
        tys = Ast.TyParamMap.empty;
        rhos = Ast.RhoParamMap.empty;
        ty_reasons = Ast.TyParamMap.empty;
        rho_reasons = Ast.RhoParamMap.empty;
      }

    let apply_ty s ty = Ast.substitute_ty s.tys s.rhos ty
    let apply_rho s rho = Ast.substitute_rho s.rhos rho
    let apply_comp_ty s ct = Ast.substitute_comp_ty s.tys s.rhos ct
    let apply_constr s c = Ast.substitute_constr s.tys s.rhos c

    let add_ty a ty reason s =
      {
        s with
        tys = Ast.TyParamMap.add a (apply_ty s ty) s.tys;
        ty_reasons = Ast.TyParamMap.add a reason s.ty_reasons;
      }

    let add_rho p rho reason s =
      {
        s with
        rhos = Ast.RhoParamMap.add p (apply_rho s rho) s.rhos;
        rho_reasons = Ast.RhoParamMap.add p reason s.rho_reasons;
      }

    let ty_reason a s = Ast.TyParamMap.find_opt a s.ty_reasons
    let rho_reason p s = Ast.RhoParamMap.find_opt p s.rho_reasons
    let find_ty a s = Ast.TyParamMap.find_opt a s.tys
    let map_ty f s = { s with tys = Ast.TyParamMap.map f s.tys }
    let map_rho f s = { s with rhos = Ast.RhoParamMap.map f s.rhos }

    let union_prefer_right s1 s2 =
      let pick _ _ v2 = Some v2 in
      {
        tys = Ast.TyParamMap.union pick s1.tys s2.tys;
        rhos = Ast.RhoParamMap.union pick s1.rhos s2.rhos;
        ty_reasons = Ast.TyParamMap.union pick s1.ty_reasons s2.ty_reasons;
        rho_reasons = Ast.RhoParamMap.union pick s1.rho_reasons s2.rho_reasons;
      }
  end

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

  (* ------------------------------------------------------------------ *)
  (* Printing for diagnostics                                            *)
  (* ------------------------------------------------------------------ *)

  (* A formatter of its own, in a horizontal box with no margin, so the result
     is a single line: a type broken across lines mid-sentence reads worse than
     a long one, and the layout is the diagnostic printer's business. *)
  let render f =
    let buffer = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer buffer in
    Format.pp_set_margin ppf 1000000;
    Format.fprintf ppf "@[<h>%t@]" f;
    Format.pp_print_flush ppf ();
    Buffer.contents buffer

  (** One pair of parameter printers per diagnostic, so that α and ρ₀ mean the
      same thing in the headline, the labels and the notes. *)
  let printer () : E.printer =
    let ty_pp = PrettyPrint.TyPrintParam.create () in
    let rho_pp = PrettyPrint.RhoPrintParam.create () in
    {
      E.ty =
        (fun ty ->
          render
            (PrettyPrint.print_ty
               (module ResourceGrade)
               ty_pp rho_pp (simplify_ty ty)));
      rho =
        (fun rho ->
          render
            (PrettyPrint.print_rho
               (module ResourceGrade)
               rho_pp (simplify_rho rho)));
      is_zero = (fun rho -> simplify_rho rho = Ast.RhoConst ResourceGrade.zero);
    }

  (* ------------------------------------------------------------------ *)
  (* Unification of types                                                *)
  (* ------------------------------------------------------------------ *)

  (** Applying [a ↦ t] to the equations still to be solved. An equation whose
      side was exactly [TyParam a] is marked as having arrived through [a], so a
      later failure can point at where [a] was decided; the first such parameter
      is kept, being the one nearest the original equation. *)
  let subst_ty_equations a t =
    let subst =
      Ast.substitute_ty (Ast.TyParamMap.singleton a t) Ast.RhoParamMap.empty
    in
    List.map (fun eq ->
        let via_lhs, via_rhs = eq.via in
        let via_lhs =
          match (via_lhs, eq.lhs) with
          | None, Ast.TyParam a' when a' = a -> Some a
          | _ -> via_lhs
        and via_rhs =
          match (via_rhs, eq.rhs) with
          | None, Ast.TyParam a' when a' = a -> Some a
          | _ -> via_rhs
        in
        let rl, rr = eq.root in
        {
          eq with
          lhs = subst eq.lhs;
          rhs = subst eq.rhs;
          root = (subst rl, subst rr);
          via = (via_lhs, via_rhs);
        })

  (* A step down keeps the parent's reason and root, so the message can still
     name the construct, with the step appended to say which part disagrees. *)
  let child eq step lhs rhs =
    {
      eq with
      lhs;
      rhs;
      reason = { eq.reason with path = reason_path eq.reason @ [ step ] };
    }

  let rec unify_ty_constraints state seen rho_eqs (eqs : ty_eq list) =
    let bind a t eq ty_eqs =
      let seen' = Subst.add_ty a t eq.reason seen in
      let ty_subst, rho_eqs' =
        unify_ty_constraints state seen' rho_eqs (subst_ty_equations a t ty_eqs)
      in
      (Subst.add_ty a t eq.reason ty_subst, rho_eqs')
    in
    match eqs with
    | [] -> (Subst.empty, rho_eqs)
    | eq :: ty_eqs when eq.lhs = eq.rhs ->
        unify_ty_constraints state seen rho_eqs ty_eqs
    | ({
         lhs = Ast.TyApply (ty_name1, args1);
         rhs = Ast.TyApply (ty_name2, args2);
         _;
       } as eq)
      :: ty_eqs
      when ty_name1 = ty_name2 ->
        let new_eqs =
          List.mapi
            (fun i (t1, t2) -> child eq (Ast.TypeArgument (i + 1)) t1 t2)
            (List.combine args1 args2)
        in
        unify_ty_constraints state seen rho_eqs (new_eqs @ ty_eqs)
    | ({ lhs = Ast.TyApply (ty_name, args); _ } as eq) :: ty_eqs
      when is_transparent_type state ty_name ->
        unify_ty_constraints state seen rho_eqs
          ({ eq with lhs = unfold state ty_name args } :: ty_eqs)
    | ({ rhs = Ast.TyApply (ty_name, args); _ } as eq) :: ty_eqs
      when is_transparent_type state ty_name ->
        unify_ty_constraints state seen rho_eqs
          ({ eq with rhs = unfold state ty_name args } :: ty_eqs)
    | ({ lhs = Ast.TyTuple tys1; rhs = Ast.TyTuple tys2; _ } as eq) :: ty_eqs
      when List.length tys1 = List.length tys2 ->
        let new_eqs =
          List.mapi
            (fun i (t1, t2) -> child eq (Ast.Component (i + 1)) t1 t2)
            (List.combine tys1 tys2)
        in
        unify_ty_constraints state seen rho_eqs (new_eqs @ ty_eqs)
    | ({
         lhs = Ast.TyArrow (t1, CompTy (t1', rho1'));
         rhs = Ast.TyArrow (t2, CompTy (t2', rho2'));
         _;
       } as eq)
      :: ty_eqs ->
        unify_ty_constraints state seen
          ({
             grade_lhs = rho1';
             grade_rhs = rho2';
             grade_reason = eq.reason;
             grade_root = Some eq.root;
           }
          :: rho_eqs)
          (child eq Ast.Argument t1 t2 :: child eq Ast.Result t1' t2' :: ty_eqs)
    | ({ lhs = Ast.TyParam a; rhs = t; _ } as eq) :: ty_eqs
      when not (occurs_ty a t) ->
        bind a t eq ty_eqs
    | ({ lhs = t; rhs = Ast.TyParam a; _ } as eq) :: ty_eqs
      when not (occurs_ty a t) ->
        bind a t eq ty_eqs
    | ({ lhs = Ast.TyBox (rho1, ty1); rhs = Ast.TyBox (rho2, ty2); _ } as eq)
      :: ty_eqs ->
        unify_ty_constraints state seen
          ({
             grade_lhs = rho1;
             grade_rhs = rho2;
             grade_reason = eq.reason;
             grade_root = Some eq.root;
           }
          :: rho_eqs)
          (child eq Ast.BoxContent ty1 ty2 :: ty_eqs)
    | ({
         lhs = Ast.TyHandler (Ast.CompTy (ty1, rho1), Ast.CompTy (ty2, rho2));
         rhs = Ast.TyHandler (Ast.CompTy (ty1', rho1'), Ast.CompTy (ty2', rho2'));
         _;
       } as eq)
      :: ty_eqs ->
        unify_ty_constraints state seen
          ({
             grade_lhs = rho1;
             grade_rhs = rho1';
             grade_reason = eq.reason;
             grade_root = Some eq.root;
           }
          :: {
               grade_lhs = rho2;
               grade_rhs = rho2';
               grade_reason = eq.reason;
               grade_root = Some eq.root;
             }
          :: rho_eqs)
          (child eq Ast.HandlerInput ty1 ty1'
          :: child eq Ast.HandlerOutput ty2 ty2'
          :: ty_eqs)
    | eq :: _ ->
        let occurs =
          match (eq.lhs, eq.rhs) with
          | Ast.TyParam a, t | t, Ast.TyParam a -> occurs_ty a t
          | _ -> false
        in
        E.ty_mismatch (printer ())
          ~ty_reason:(fun a -> Subst.ty_reason a seen)
          ~lhs:eq.lhs ~rhs:eq.rhs ~reason:eq.reason ~root:eq.root ~via:eq.via
          ~occurs

  (* ------------------------------------------------------------------ *)
  (* Unification of grades                                               *)
  (* ------------------------------------------------------------------ *)

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
      congruence, and the same for [≾], so every solution of the cancelled
      constraint is a solution of the original one. The converse needs the
      monoid to be cancellative, which sets of timed traces are not:
      [{ε, a} · {ε, a, aa} = {ε, a} · {ε, aa}] even though
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

  let subst_rho_equations subst =
    List.map (fun eq ->
        {
          eq with
          grade_lhs = Ast.substitute_rho subst eq.grade_lhs;
          grade_rhs = Ast.substitute_rho subst eq.grade_rhs;
        })

  let rec unify_rho_constraints state prev_unsolved_size unsolved
      (eqs : rho_eq list) =
    match eqs with
    | [] ->
        let current_unsolved_size = List.length unsolved in
        if current_unsolved_size = 0 then
          (* All constraints solved *)
          Subst.empty
        else if current_unsolved_size = prev_unsolved_size then
          let sorted =
            List.stable_sort
              (fun e1 e2 ->
                Location.compare
                  (reason_at e1.grade_reason)
                  (reason_at e2.grade_reason))
              unsolved
          in
          E.rho_stuck (printer ())
            (List.map
               (fun eq ->
                 (eq.grade_lhs, eq.grade_rhs, eq.grade_reason, eq.grade_root))
               sorted)
        else
          (* Retry with deferred constraints *)
          unify_rho_constraints state current_unsolved_size [] unsolved
    | eq :: eqs -> (
        let rho1' = simplify_rho eq.grade_lhs in
        let rho2' = simplify_rho eq.grade_rhs in
        let defer left right =
          { eq with grade_lhs = left; grade_rhs = right }
        in
        let eliminate tp rho =
          let singleton = Ast.RhoParamMap.singleton tp rho in
          let rho_subst =
            unify_rho_constraints state prev_unsolved_size
              (subst_rho_equations singleton unsolved)
              (subst_rho_equations singleton eqs)
          in
          Subst.add_rho tp rho eq.grade_reason rho_subst
        in
        match (rho1', rho2') with
        | _ when rho1' = rho2' ->
            unify_rho_constraints state prev_unsolved_size unsolved eqs
        | Ast.RhoParam tp, rho when not (occurs_rho tp rho) -> eliminate tp rho
        | rho, Ast.RhoParam tp when not (occurs_rho tp rho) -> eliminate tp rho
        | Ast.RhoConst z, Ast.RhoAdd (t1, t2)
        | Ast.RhoAdd (t1, t2), Ast.RhoConst z
          when z = ResourceGrade.zero ->
            unify_rho_constraints state prev_unsolved_size unsolved
              (defer t1 (Ast.RhoConst ResourceGrade.zero)
              :: defer t2 (Ast.RhoConst ResourceGrade.zero)
              :: eqs)
        | t, (Ast.RhoAdd _ as u) ->
            let left_rho, right_rho = normalise_rho_pair t u in
            if left_rho = t && right_rho = u then
              unify_rho_constraints state prev_unsolved_size
                (defer left_rho right_rho :: unsolved)
                eqs
            else
              unify_rho_constraints state prev_unsolved_size unsolved
                (defer left_rho right_rho :: eqs)
        | (Ast.RhoAdd _ as u), t ->
            let left_rho, right_rho = normalise_rho_pair u t in
            if left_rho = u && right_rho = t then
              unify_rho_constraints state prev_unsolved_size
                (defer left_rho right_rho :: unsolved)
                eqs
            else
              unify_rho_constraints state prev_unsolved_size unsolved
                (defer left_rho right_rho :: eqs)
        | (Ast.RhoRigid _ as r), u | u, (Ast.RhoRigid _ as r) ->
            (* Nothing above applied, so the other side is neither an unknown
               nor a reducible sum: the case would be well-typed only for this
               one grade of its continuation. *)
            E.rigid_required_equal (printer ()) ~rigid:r ~other:u
              ~reason:eq.grade_reason ~root:eq.grade_root
        | u1, u2 ->
            unify_rho_constraints state prev_unsolved_size
              (defer u1 u2 :: unsolved) eqs)

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

  let subst_rho_inequations subst = List.map (Subst.apply_constr subst)

  let rec unify_rho_ineq_constraints state prev_unsolved_size unsolved =
    let process ~may_default wrap reason rho1 rho2 ineqs =
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
          let singleton = Subst.add_rho tp rho reason Subst.empty in
          let rho_subst, unsolved' =
            unify_rho_ineq_constraints state prev_unsolved_size
              (subst_rho_inequations singleton unsolved)
              (subst_rho_inequations singleton ineqs)
          in
          (Subst.add_rho tp rho reason rho_subst, unsolved')
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
          (Subst.empty, unsolved)
        else
          (* Retry with deferred constraints *)
          unify_rho_ineq_constraints state current_unsolved_size [] unsolved
    | Ast.EternalOrIneq (ty, rho1, rho2, reason) :: ineqs ->
        process
          ~may_default:(reduce_eternal state ty = None)
          (fun r1 r2 -> Ast.EternalOrIneq (ty, r1, r2, reason))
          reason rho1 rho2 ineqs
    | Ast.Ineq (rho1, rho2, reason) :: ineqs ->
        process ~may_default:true
          (fun r1 r2 -> Ast.Ineq (r1, r2, reason))
          reason rho1 rho2 ineqs
    | (Ast.Eternal _ as c) :: ineqs ->
        unify_rho_ineq_constraints state prev_unsolved_size (c :: unsolved)
          ineqs

  (** The cost model the sub-grade order of the timed-trace grades reads: the
      runtime bounds declared by the operation named by a trace event. Only the
      grades of operation signatures have their events validated when they are
      declared (see {!add_operation_signature}); the events of any other grade
      literal — a [box] grade or a grade written in a type — are checked lazily,
      here, the first time the order needs their cost. *)
  let op_bounds ~loc state ev =
    match StringMap.find_opt ev state.op_bounds with
    | Some bounds -> bounds
    | None ->
        Error.typing ~loc
          "Unknown event '%s'; the events of a resource grade must be declared \
           operations"
          ev

  (** The verdict on an inequality [ρ₁ ≾ ρ₂]. Rigid grades in it are universally
      quantified, so [Holds] means it holds for every grade they may take, and
      [Fails (Some w)] that it already fails when they are all [w]. [Unknown] is
      left open by the unknown grades. *)
  type verdict = Holds | Fails of ResourceGrade.t option | Unknown

  (** [decide_ineq state ~loc rho1 rho2] decides [rho1 ≾ rho2] as far as the
      unknown grades allow, never guessing at them. To prove it,
      {!normalise_rho_pair} cancels the common prefix and suffix, and the rigid
      grades are dropped from the side where that only strengthens the
      inequality: the greater side when the unit is the minimum of the order,
      the smaller side when it is the top. If what remains is ground, it
      decides. To refute it, the rigid grades are instantiated by a witness —
      the unit, one tick, or one tick past all the constants — since a failing
      ground instance refutes the universal statement. *)
  let decide_ineq ~loc state rho1 rho2 =
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
              Some (ResourceGrade.is_sub_rho (op_bounds ~loc state) v1 v2)
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
  let simplify_constraints state cs =
    let mentions_rigid rho =
      not (Ast.RhoParamSet.is_empty (Ast.rigid_rhos rho))
    in
    let simplify acc = function
      | Ast.Ineq (rho1, rho2, reason) -> (
          let rho1 = simplify_rho rho1 and rho2 = simplify_rho rho2 in
          match decide_ineq ~loc:(reason_at reason) state rho1 rho2 with
          | Holds -> acc
          | Fails witness -> E.ineq_failed (printer ()) rho1 rho2 reason witness
          | Unknown -> Ast.Ineq (rho1, rho2, reason) :: acc)
      | Ast.Eternal (ty, reason) as c -> (
          match reduce_eternal state ty with
          | None -> E.not_eternal (printer ()) ty reason
          | Some vars when Ast.TyParamSet.is_empty vars -> acc
          | Some _ -> c :: acc)
      | Ast.EternalOrIneq (ty, rho1, rho2, reason) -> (
          let rho1 = simplify_rho rho1 and rho2 = simplify_rho rho2 in
          match decide_ineq ~loc:(reason_at reason) state rho1 rho2 with
          | Holds -> acc
          | verdict -> (
              match (reduce_eternal state ty, verdict) with
              | Some vars, _ when Ast.TyParamSet.is_empty vars -> acc
              | None, Fails witness ->
                  E.eternal_or_ineq_failed (printer ()) ty rho1 rho2 reason
                    witness
              | None, _ ->
                  E.eternal_or_ineq_unknown (printer ()) ty rho1 rho2 reason
              | Some _, Fails _ -> Ast.Eternal (ty, reason) :: acc
              | Some _, _ ->
                  if mentions_rigid rho1 || mentions_rigid rho2 then
                    Ast.Eternal (ty, reason) :: acc
                  else Ast.EternalOrIneq (ty, rho1, rho2, reason) :: acc))
    in
    List.rev (List.fold_left simplify [] cs)

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
      variables its type reduces to, and no constraint implied by another.

      Canonical forms are compared on their types and grades alone, so a
      duplicate keeps the reason of the constraint that was seen first. *)
  let solve_residuals state ~generalisable:(gen_tys, gen_rhos) cs =
    let fv_tys, fv_rhos =
      List.fold_left
        (fun (tys, rhos) c ->
          let tys', rhos' = Ast.free_vars_constr c in
          (Ast.TyParamSet.union tys tys', Ast.RhoParamSet.union rhos rhos'))
        (Ast.TyParamSet.empty, Ast.RhoParamSet.empty)
        cs
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
    let cs' =
      simplify_constraints state
        (List.map (Ast.substitute_constr ty_subst rho_subst) cs)
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
          | Ast.Ineq (rho1, rho2, reason) ->
              E.non_ground_ineq (printer ()) rho1 rho2 reason
          | Ast.Eternal (ty, reason) ->
              ( List.fold_left
                  (fun atoms a ->
                    if List.mem_assoc a atoms then atoms
                    else atoms @ [ (a, reason) ])
                  atoms (vars_of ty),
                disjs )
          | Ast.EternalOrIneq (ty, rho1, rho2, reason) ->
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
                      Ast.EternalOrIneq (canonical_ty vars, rho1, rho2, reason);
                    ] ))
        ([], []) cs'
    in
    let implied = function
      | Ast.EternalOrIneq (ty, _, _, _) ->
          List.for_all (fun a -> List.mem_assoc a eternal_atoms) (vars_of ty)
      | _ -> false
    in
    List.map
      (fun (a, reason) -> Ast.Eternal (Ast.TyParam a, reason))
      eternal_atoms
    @ List.filter (fun c -> not (implied c)) disjunctions

  (** The solver visits the constraints in the order the program reads, so that
      the first failure it meets is the first one a reader would. The file name
      is compared too: a constraint inherited from another scheme may point
      elsewhere. *)
  let in_source_order cs =
    let by at1 at2 = Location.compare at1 at2 in
    {
      ty_eqs =
        List.stable_sort
          (fun e1 e2 -> by (reason_at e1.reason) (reason_at e2.reason))
          cs.ty_eqs;
      rho_eqs =
        List.stable_sort
          (fun e1 e2 ->
            by (reason_at e1.grade_reason) (reason_at e2.grade_reason))
          cs.rho_eqs;
      ineqs =
        List.stable_sort
          (fun c1 c2 ->
            by (reason_at (constr_reason c1)) (reason_at (constr_reason c2)))
          cs.ineqs;
    }

  let unify state cs =
    let cs = in_source_order cs in
    let ty_subst, rho_eqs' =
      unify_ty_constraints state Subst.empty [] cs.ty_eqs
    in
    let rho_subst = unify_rho_constraints state 0 [] (cs.rho_eqs @ rho_eqs') in
    let ineqs' =
      subst_rho_inequations
        (Subst.union_prefer_right ty_subst rho_subst)
        cs.ineqs
    in
    let rho_subst', ineqs'' = unify_rho_ineq_constraints state 0 [] ineqs' in
    let rho_subst'' =
      Subst.union_prefer_right rho_subst'
        (Subst.map_rho (Subst.apply_rho rho_subst') rho_subst)
    in
    let ty_subst' =
      Subst.map_ty
        (Subst.apply_ty (Subst.union_prefer_right ty_subst rho_subst''))
        ty_subst
    in
    let subst = Subst.union_prefer_right ty_subst' rho_subst'' in
    let residual =
      simplify_constraints state (subst_rho_inequations subst ineqs'')
    in
    (subst, residual)

  (** A rigid grade is universally quantified in the handler case that
      introduced it, so it must not escape: in the type of a definition it would
      be generalised and instantiated freely at each use. *)
  let check_no_rigid_escape ~loc rigid describe =
    match Ast.RhoParamSet.choose_opt rigid with
    | None -> ()
    | Some r ->
        let rho_pp = PrettyPrint.RhoPrintParam.create () in
        let ty_pp = PrettyPrint.TyPrintParam.create () in
        Error.typing ~loc
          "The grade %t of a handler continuation may be any grade and cannot \
           occur in %t"
          (PrettyPrint.print_rho (module ResourceGrade) rho_pp (Ast.RhoRigid r))
          (describe ty_pp rho_pp)

  let infer ~(loc : Location.t) state e =
    let comp_ty, cs = infer_computation state e in
    let subst, residual = unify state cs in
    (* A top-level computation exports no type, so every parameter of its
       residual constraints may be instantiated as the constraints need. *)
    let _ =
      solve_residuals state
        ~generalisable:(Ast.TyParamSet.empty, Ast.RhoParamSet.empty)
        residual
    in
    let comp_ty' = simplify_comp_ty (Subst.apply_comp_ty subst comp_ty) in
    (let (Ast.CompTy (ty, rho)) = comp_ty' in
     check_no_rigid_escape ~loc (Ast.rigid_rhos_comp_ty comp_ty')
       (fun ty_pp rho_pp ppf ->
         Format.fprintf ppf "the type %t # %t of the computation"
           (PrettyPrint.print_ty (module ResourceGrade) ty_pp rho_pp ty)
           (PrettyPrint.print_rho (module ResourceGrade) rho_pp rho)));
    comp_ty'

  let add_external_function x entry state =
    {
      state with
      variables = ContextHolderModule.add_variable x entry state.variables;
    }

  let add_top_definition ~(loc : Location.t) state x e =
    let ty, cs = infer_expression state e in
    let subst, residual = unify state cs in
    let ty'' = simplify_ty (Subst.apply_ty subst ty) in
    check_no_rigid_escape ~loc (Ast.rigid_rhos_ty ty'') (fun ty_pp rho_pp ppf ->
        Format.fprintf ppf "the type %t of %t"
          (PrettyPrint.print_ty (module ResourceGrade) ty_pp rho_pp ty'')
          (Ast.Variable.print x));
    let free_vars, free_rhos = Ast.free_vars ty'' in
    (* The constraints the definition could not discharge qualify its scheme,
       to be owed again at each use, which nests the definition's reason for
       each inside its own. *)
    let constrs =
      solve_residuals state ~generalisable:(free_vars, free_rhos) residual
    in
    let scheme : ResourceGrade.t Ast.ty_scheme =
      {
        ty_params = Ast.TyParamSet.elements free_vars;
        rho_params = Ast.RhoParamSet.elements free_rhos;
        constrs;
        ty = ty'';
      }
    in
    add_external_function x
      { scheme; scope = Global; bound_at = Some loc }
      state

  (* A top-level definition whose type could not be inferred, given the scheme
     [∀α. α]. It lets the loader carry on after an ill-typed definition without
     every later use of it failing again as an unknown variable. *)
  let assume_definition ~(loc : Location.t) state x =
    let a = Ast.TyParamModule.fresh "assumed" in
    add_external_function x
      {
        scheme =
          {
            ty_params = [ a ];
            rho_params = [];
            constrs = [];
            ty = Ast.TyParam a;
          };
        scope = Global;
        bound_at = Some loc;
      }
      state

  (* An alias is unfolded by the unifier before the eternality check ever sees
     it, so a [noneternal] flag on one could not be honoured; reject it rather
     than silently ignore it. *)
  let check_noneternal_definable ~loc (_, ty_name, ty_def) =
    match ty_def with
    | Ast.TySum _ -> ()
    | Ast.TyInline _ ->
        let name = Ast.TyName.string_of ty_name in
        Error.typing ~loc
          "type %s is an alias and cannot be declared noneternal; wrap it in a \
           constructor, as in 'noneternal type %s = %s of ...'"
          name name
          (String.capitalize_ascii name)

  let add_type_definitions ~(loc : Location.t) state (eternality, ty_defs) =
    (match eternality with
    | Ast.Derived -> ()
    | Ast.Noneternal -> List.iter (check_noneternal_definable ~loc) ty_defs);
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
    List.iter (fun (_, _, ty_def) -> check_ty_def ~loc state' ty_def) ty_defs;
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
  let add_operation_signature ~(loc : Location.t) state
      (op, ty1, ty2, rho, bounds) =
    let op_name = Ast.OpName.string_of op in
    let event_bounds ev =
      match StringMap.find_opt ev state.op_bounds with
      | Some bounds -> bounds
      | None ->
          Error.typing ~loc "unknown event '%s' in the grade of operation %s" ev
            op_name
    in
    let op_bounds' =
      match (ResourceGrade.needs_op_bounds, rho, bounds) with
      | false, _, Some _ ->
          Error.typing ~loc
            "runtime bounds are only used by the timed-trace grading monoids; \
             under '%s' the operation grade already carries them"
            ResourceGrade.name
      | false, _, None -> state.op_bounds
      | true, (Ast.RhoParam _ | Ast.RhoRigid _ | Ast.RhoAdd _), _ ->
          Error.typing ~loc
            "the grade of operation %s must be a literal under the '%s' \
             grading monoid"
            op_name ResourceGrade.name
      | true, Ast.RhoConst grade, _ when ResourceGrade.is_atomic op_name grade
        -> (
          match bounds with
          | None ->
              Error.typing ~loc
                "atomic operation %s needs runtime bounds `within (lo, hi)` \
                 under the '%s' grading monoid"
                op_name ResourceGrade.name
          | Some (lo, hi) ->
              if lo > hi then
                Error.typing ~loc
                  "the runtime bounds of operation %s must satisfy lo <= hi"
                  op_name
              else if hi < 1 then
                Error.typing ~loc
                  "the upper runtime bound of operation %s must be at least 1"
                  op_name
              else StringMap.add op_name (lo, hi) state.op_bounds)
      | true, Ast.RhoConst grade, Some _ ->
          Error.typing ~loc
            "operation %s is compound, so its runtime bounds follow from its \
             grade %s and must not be declared"
            op_name (ResourceGrade.show grade)
      | true, Ast.RhoConst grade, None -> (
          if List.mem op_name (ResourceGrade.events grade) then
            Error.typing ~loc
              "compound operation %s may not name itself in its grade %s"
              op_name (ResourceGrade.show grade);
          match ResourceGrade.implied_bounds event_bounds grade with
          | Some bounds -> StringMap.add op_name bounds state.op_bounds
          | None -> state.op_bounds)
    in
    {
      state with
      op_signatures =
        Ast.OpNameMap.add op (ty1, ty2, rho, loc) state.op_signatures;
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
  let add_operation_default ~(loc : Location.t) state (op, abs) =
    let op_name = Ast.OpName.string_of op in
    match Ast.OpNameMap.find_opt op state.op_signatures with
    | None -> Error.typing ~loc "unknown operation %s" op_name
    | Some (param_ty, arity_ty, op_rho, signature_at) ->
        if Ast.OpNameSet.mem op state.op_defaults then
          Error.typing ~loc "operation %s already has a default implementation"
            op_name;
        (match op_rho with
        | Ast.RhoConst grade when not (ResourceGrade.is_atomic op_name grade) ->
            Error.typing ~loc
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
        let arg_ty, CompTy (res_ty, impl_rho), cs =
          infer_abstraction state abs
        in
        (* The two equations of a default share a reason, so the step that
           tells them apart is recorded in its path. *)
        let default path : reason =
          { at = loc; why = Ast.DefaultOf { op; signature_at }; path }
        in
        let _, residual =
          unify state
            (concat
               [
                 ty_eq (default [ Ast.Argument ]) arg_ty param_ty;
                 ty_eq (default [ Ast.Result ]) res_ty arity_ty;
                 cs;
                 ineq (default []) impl_rho bound_rho;
               ])
        in
        let _ =
          solve_residuals state
            ~generalisable:(Ast.TyParamSet.empty, Ast.RhoParamSet.empty)
            residual
        in
        { state with op_defaults = Ast.OpNameSet.add op state.op_defaults }

  let load_primitive state x prim =
    let ty_params, rho_params, ty = P.primitive_type_scheme prim in
    add_external_function x
      {
        scheme = { ty_params; rho_params; constrs = []; ty };
        scope = Global;
        bound_at = None;
      }
      state
end
