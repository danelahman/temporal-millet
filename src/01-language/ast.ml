module Common = struct
  module Symbol = Utils.Symbol
  module Variable = Symbol.Make ()
  module VariableMap = Map.Make (Variable)
  module Label = Symbol.Make ()
  module TyName = Symbol.Make ()

  type ty_name = TyName.t

  module TyNameMap = Map.Make (TyName)
  module TyParam = Symbol.Make ()
  module TyParamMap = Map.Make (TyParam)
  module TyParamSet = Set.Make (TyParam)

  type ty_param = TyParam.t

  module EffectGradeParam = Symbol.Make ()
  module EffectGradeParamMap = Map.Make (EffectGradeParam)
  module EffectGradeParamSet = Set.Make (EffectGradeParam)

  type effect_grade_param = EffectGradeParam.t

  module ResourceGradeParam = Symbol.Make ()
  module ResourceGradeParamMap = Map.Make (ResourceGradeParam)
  module ResourceGradeParamSet = Set.Make (ResourceGradeParam)

  type resource_grade_param = ResourceGradeParam.t

  module OpName = Symbol.Make ()
  module OpNameMap = Map.Make (OpName)

  type operation = OpName.t
  type variable = Variable.t
  type label = Label.t

  let bool_ty_name = TyName.fresh "bool"
  let int_ty_name = TyName.fresh "int"
  let unit_ty_name = TyName.fresh "unit"
  let string_ty_name = TyName.fresh "string"
  let float_ty_name = TyName.fresh "float"
  let list_ty_name = TyName.fresh "list"
  let empty_ty_name = TyName.fresh "empty"
  let nil_label_string = "$nil$"
  let nil_label = Label.fresh nil_label_string
  let cons_label_string = "$cons$"
  let cons_label = Label.fresh cons_label_string
end

include Common

module Make (GS : GradeSystem.S) = struct
  include Common

  type effect_grade =
    | EffectGradeConst of GS.EffectGrade.t
    | EffectGradeParam of effect_grade_param
    | EffectGradeAdd of effect_grade * effect_grade

  type resource_grade =
    | ResourceGradeConst of GS.ResourceGrade.t
    | ResourceGradeParam of resource_grade_param
    | ResourceGradeAdd of resource_grade * resource_grade
    | ResourceGradeOfEffect of effect_grade

  type ty =
    | TyConst of Const.ty
    | TyApply of ty_name * ty list  (** [(ty1, ty2, ..., tyn) type_name] *)
    | TyParam of ty_param  (** ['a] *)
    | TyArrow of ty * comp_ty  (** [ty1 -> ty2 ! rho] *)
    | TyTuple of ty list  (** [ty1 * ty2 * ... * tyn] *)
    | TyBox of resource_grade * ty  (** [ [rho]ty ] *)
    | TyHandler of comp_ty * comp_ty

  and comp_ty = CompTy of ty * effect_grade  (** [ty ! rho] *)

  type pattern =
    | PVar of variable
    | PAnnotated of pattern * ty
    | PAs of pattern * variable
    | PTuple of pattern list
    | PVariant of label * pattern option
    | PConst of Const.t
    | PNonbinding

  type expression =
    | Var of variable
    | Const of Const.t
    | Annotated of expression * ty
    | Tuple of expression list
    | Variant of label * expression option
    | Lambda of abstraction
    | PureLambda of abstraction
    | RecLambda of variable * abstraction
    | Handler of abstraction * abstraction OpNameMap.t

  and computation =
    | Return of expression
    | Do of computation * abstraction
    | Match of expression * abstraction list
    | Apply of expression * expression
    | Delay of int * computation
    | Box of resource_grade * expression * abstraction
    | Unbox of expression * abstraction
    | Perform of operation * expression * abstraction
    | Handle of computation * expression

  and abstraction = pattern * computation

  type ty_def = TySum of (label * ty option) list | TyInline of ty

  type command =
    | TyDef of (ty_param list * ty_name * ty_def) list
    | OpSig of (operation * ty * ty * effect_grade)
    | TopLet of variable * expression
    | TopDo of computation
    | Grades of string

  type 'a context_elem_ty =
    | VarMap of 'a VariableMap.t
    | ResourceGrade of resource_grade

  type 'a context = 'a context_elem_ty list

  let rec substitute_effect_grade eps_subst = function
    | EffectGradeConst _ as eps -> eps
    | EffectGradeParam ep as eps -> (
        match EffectGradeParamMap.find_opt ep eps_subst with
        | None -> eps
        | Some eps' -> eps')
    | EffectGradeAdd (eps1, eps2) ->
        EffectGradeAdd
          ( substitute_effect_grade eps_subst eps1,
            substitute_effect_grade eps_subst eps2 )

  let rec substitute_resource_grade rho_subst eps_subst = function
    | ResourceGradeConst _ as rho -> rho
    | ResourceGradeParam tp as rho -> (
        match ResourceGradeParamMap.find_opt tp rho_subst with
        | None -> rho
        | Some rho' -> rho')
    | ResourceGradeAdd (rho, rho') ->
        ResourceGradeAdd
          ( substitute_resource_grade rho_subst eps_subst rho,
            substitute_resource_grade rho_subst eps_subst rho' )
    | ResourceGradeOfEffect eps ->
        ResourceGradeOfEffect (substitute_effect_grade eps_subst eps)

  let rec substitute_ty ty_subst rho_subst eps_subst = function
    | TyConst _ as ty -> ty
    | TyParam a as ty -> (
        match TyParamMap.find_opt a ty_subst with None -> ty | Some ty' -> ty')
    | TyApply (ty_name, tys) ->
        TyApply
          (ty_name, List.map (substitute_ty ty_subst rho_subst eps_subst) tys)
    | TyTuple tys ->
        TyTuple (List.map (substitute_ty ty_subst rho_subst eps_subst) tys)
    | TyArrow (ty1, cty2) ->
        TyArrow
          ( substitute_ty ty_subst rho_subst eps_subst ty1,
            substitute_comp_ty ty_subst rho_subst eps_subst cty2 )
    | TyBox (rho, ty) ->
        TyBox
          ( substitute_resource_grade rho_subst eps_subst rho,
            substitute_ty ty_subst rho_subst eps_subst ty )
    | TyHandler (cty1, cty2) ->
        TyHandler
          ( substitute_comp_ty ty_subst rho_subst eps_subst cty1,
            substitute_comp_ty ty_subst rho_subst eps_subst cty2 )

  and substitute_comp_ty ty_subst rho_subst eps_subst = function
    | CompTy (ty, eps) ->
        CompTy
          ( substitute_ty ty_subst rho_subst eps_subst ty,
            substitute_effect_grade eps_subst eps )

  let rec free_effect_grades = function
    | EffectGradeConst _ -> EffectGradeParamSet.empty
    | EffectGradeParam ep -> EffectGradeParamSet.singleton ep
    | EffectGradeAdd (eps1, eps2) ->
        EffectGradeParamSet.union (free_effect_grades eps1)
          (free_effect_grades eps2)

  let rec free_resource_grades = function
    | ResourceGradeConst _ ->
        (ResourceGradeParamSet.empty, EffectGradeParamSet.empty)
    | ResourceGradeParam a ->
        (ResourceGradeParamSet.singleton a, EffectGradeParamSet.empty)
    | ResourceGradeAdd (l, r) ->
        let rho_l, eps_l = free_resource_grades l in
        let rho_r, eps_r = free_resource_grades r in
        ( ResourceGradeParamSet.union rho_l rho_r,
          EffectGradeParamSet.union eps_l eps_r )
    | ResourceGradeOfEffect eps ->
        (ResourceGradeParamSet.empty, free_effect_grades eps)

  let empty_free_vars =
    (TyParamSet.empty, ResourceGradeParamSet.empty, EffectGradeParamSet.empty)

  let union_free_vars (ty1, rho1, eps1) (ty2, rho2, eps2) =
    ( TyParamSet.union ty1 ty2,
      ResourceGradeParamSet.union rho1 rho2,
      EffectGradeParamSet.union eps1 eps2 )

  let rec free_vars = function
    | TyConst _ -> empty_free_vars
    | TyParam a ->
        ( TyParamSet.singleton a,
          ResourceGradeParamSet.empty,
          EffectGradeParamSet.empty )
    | TyApply (_, tys) | TyTuple tys ->
        List.fold_left
          (fun acc ty -> union_free_vars acc (free_vars ty))
          empty_free_vars tys
    | TyArrow (ty, cty) ->
        union_free_vars (free_vars ty) (free_comp_ty_vars cty)
    | TyBox (rho, ty) ->
        let fv_ty, fv_rho, fv_eps = free_vars ty in
        let nested_rho, nested_eps = free_resource_grades rho in
        ( fv_ty,
          ResourceGradeParamSet.union fv_rho nested_rho,
          EffectGradeParamSet.union fv_eps nested_eps )
    | TyHandler (cty1, cty2) ->
        union_free_vars (free_comp_ty_vars cty1) (free_comp_ty_vars cty2)

  and free_comp_ty_vars = function
    | CompTy (ty, eps) ->
        let fv_ty, fv_rho, fv_eps = free_vars ty in
        ( fv_ty,
          fv_rho,
          EffectGradeParamSet.union fv_eps (free_effect_grades eps) )
end
