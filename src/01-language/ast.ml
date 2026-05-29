module Symbol = Utils.Symbol
module Variable = Symbol.Make ()
module VariableMap = Map.Make (Variable)
module Label = Symbol.Make ()
module TyName = Symbol.Make ()

type ty_name = TyName.t

module TyNameMap = Map.Make (TyName)
module TyParamModule = Symbol.Make ()
module TyParamMap = Map.Make (TyParamModule)
module TyParamSet = Set.Make (TyParamModule)

type ty_param = TyParamModule.t

module ResourceGradeParamModule = Symbol.Make ()
module ResourceGradeParamMap = Map.Make (ResourceGradeParamModule)
module ResourceGradeParamSet = Set.Make (ResourceGradeParamModule)

type resource_grade_param = ResourceGradeParamModule.t

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

(** The grade-dependent AST. [Make] is parameterised by a whole grade system
    rather than just by the type of resource grade constants, so that the types
    can refer to both [GS.ResourceGrade.t] and (in the future)
    [GS.EffectGrade.t] constants. The grade-independent definitions above
    (symbol modules, type names, the built-in [*_ty_name] constants, …) live
    outside the functor and are re-exported below so that they keep a single
    shared identity across all grade-system instantiations. *)
module Make (GS : GradeSystem.S) = struct
  (* Re-export the grade-independent definitions so that a single
     [module Ast = Language.Ast.Make (GS)] gives access to the whole AST. *)
  module Symbol = Symbol
  module Variable = Variable
  module VariableMap = VariableMap
  module Label = Label
  module TyName = TyName
  module TyNameMap = TyNameMap
  module TyParamModule = TyParamModule
  module TyParamMap = TyParamMap
  module TyParamSet = TyParamSet
  module ResourceGradeParamModule = ResourceGradeParamModule
  module ResourceGradeParamMap = ResourceGradeParamMap
  module ResourceGradeParamSet = ResourceGradeParamSet
  module OpName = OpName
  module OpNameMap = OpNameMap

  type nonrec ty_name = ty_name
  type nonrec ty_param = ty_param
  type nonrec resource_grade_param = resource_grade_param
  type nonrec operation = operation
  type nonrec variable = variable
  type nonrec label = label

  let bool_ty_name = bool_ty_name
  let int_ty_name = int_ty_name
  let unit_ty_name = unit_ty_name
  let string_ty_name = string_ty_name
  let float_ty_name = float_ty_name
  let list_ty_name = list_ty_name
  let empty_ty_name = empty_ty_name
  let nil_label_string = nil_label_string
  let nil_label = nil_label
  let cons_label_string = cons_label_string
  let cons_label = cons_label

  type resource_grade =
    | ResourceGradeConst of GS.ResourceGrade.t
    | ResourceGradeParam of resource_grade_param
    | ResourceGradeAdd of resource_grade * resource_grade

  type ty =
    | TyConst of Const.ty
    | TyApply of ty_name * ty list  (** [(ty1, ty2, ..., tyn) type_name] *)
    | TyParam of ty_param  (** ['a] *)
    | TyArrow of ty * comp_ty  (** [ty1 -> ty2 ! rho] *)
    | TyTuple of ty list  (** [ty1 * ty2 * ... * tyn] *)
    | TyBox of resource_grade * ty  (** [ [rho]ty ] *)
    | TyHandler of comp_ty * comp_ty

  and comp_ty = CompTy of ty * resource_grade  (** [ty ! rho] *)

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
    | OpSig of (operation * ty * ty * resource_grade)
    | TopLet of variable * expression
    | TopDo of computation
    | Grades of string

  type ('var, 'map, 'resource_grade) context_elem_ty =
    | VarMap of 'map
    | ResourceGrade of 'resource_grade

  type ('var, 'map, 'resource_grade) context =
    ('var, 'map, 'resource_grade) context_elem_ty list

  let rec substitute_resource_grade subst = function
    | ResourceGradeConst _ as rho -> rho
    | ResourceGradeParam tp as rho -> (
        match ResourceGradeParamMap.find_opt tp subst with
        | None -> rho
        | Some rho' -> rho')
    | ResourceGradeAdd (rho, rho') ->
        ResourceGradeAdd
          ( substitute_resource_grade subst rho,
            substitute_resource_grade subst rho' )

  let rec substitute_ty ty_subst rho_subst = function
    | TyConst _ as ty -> ty
    | TyParam a as ty -> (
        match TyParamMap.find_opt a ty_subst with None -> ty | Some ty' -> ty')
    | TyApply (ty_name, tys) ->
        TyApply (ty_name, List.map (substitute_ty ty_subst rho_subst) tys)
    | TyTuple tys -> TyTuple (List.map (substitute_ty ty_subst rho_subst) tys)
    | TyArrow (ty1, CompTy (ty2, rho)) ->
        TyArrow
          ( substitute_ty ty_subst rho_subst ty1,
            CompTy
              ( substitute_ty ty_subst rho_subst ty2,
                substitute_resource_grade rho_subst rho ) )
    | TyBox (rho, ty) ->
        TyBox
          ( substitute_resource_grade rho_subst rho,
            substitute_ty ty_subst rho_subst ty )
    | TyHandler (CompTy (ty1, rho1), CompTy (ty2, rho2)) ->
        TyHandler
          ( CompTy
              ( substitute_ty ty_subst rho_subst ty1,
                substitute_resource_grade rho_subst rho1 ),
            CompTy
              ( substitute_ty ty_subst rho_subst ty2,
                substitute_resource_grade rho_subst rho2 ) )

  let substitute_comp_ty ty_subst rho_subst = function
    | CompTy (ty, rho) ->
        CompTy
          ( substitute_ty ty_subst rho_subst ty,
            substitute_resource_grade rho_subst rho )

  let rec free_vars = function
    | TyConst _ -> (TyParamSet.empty, ResourceGradeParamSet.empty)
    | TyParam a -> (TyParamSet.singleton a, ResourceGradeParamSet.empty)
    | TyApply (_, tys) ->
        List.fold_left
          (fun (ty_params, rho_params) ty ->
            let fv_ty, fv_rho = free_vars ty in
            ( TyParamSet.union ty_params fv_ty,
              ResourceGradeParamSet.union rho_params fv_rho ))
          (TyParamSet.empty, ResourceGradeParamSet.empty)
          tys
    | TyTuple tys ->
        List.fold_left
          (fun (ty_params, rho_params) ty ->
            let fv_ty, fv_rho = free_vars ty in
            ( TyParamSet.union ty_params fv_ty,
              ResourceGradeParamSet.union rho_params fv_rho ))
          (TyParamSet.empty, ResourceGradeParamSet.empty)
          tys
    | TyArrow (ty1, CompTy (ty2, rho)) ->
        let fv_ty1, fv_rho1 = free_vars ty1 in
        let fv_ty2, fv_rho2 = free_vars ty2 in
        let nested_free_rhos = free_resource_grades rho in
        ( TyParamSet.union fv_ty1 fv_ty2,
          ResourceGradeParamSet.union
            (ResourceGradeParamSet.union fv_rho1 fv_rho2)
            nested_free_rhos )
    | TyBox (rho, ty) ->
        let fv_ty, fv_rho = free_vars ty in
        let nested_free_rhos = free_resource_grades rho in
        (fv_ty, ResourceGradeParamSet.union fv_rho nested_free_rhos)
    | TyHandler (CompTy (ty1, rho1), CompTy (ty2, rho2)) ->
        let fv_ty1, fv_rho1 = free_vars ty1 in
        let fv_ty2, fv_rho2 = free_vars ty2 in
        let nested_free_rhos1 = free_resource_grades rho1 in
        let nested_free_rhos2 = free_resource_grades rho2 in
        ( TyParamSet.union fv_ty1 fv_ty2,
          ResourceGradeParamSet.union
            (ResourceGradeParamSet.union fv_rho1 fv_rho2)
            (ResourceGradeParamSet.union nested_free_rhos1 nested_free_rhos2) )

  and free_resource_grades rho =
    match rho with
    | ResourceGradeConst _ -> ResourceGradeParamSet.empty
    | ResourceGradeParam a -> ResourceGradeParamSet.singleton a
    | ResourceGradeAdd (l, r) ->
        ResourceGradeParamSet.union (free_resource_grades l)
          (free_resource_grades r)
end
