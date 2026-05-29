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

type 'a resource_grade =
  | ResourceGradeConst of 'a
  | ResourceGradeParam of resource_grade_param
  | ResourceGradeAdd of 'a resource_grade * 'a resource_grade

type 'a ty =
  | TyConst of Const.ty
  | TyApply of ty_name * 'a ty list  (** [(ty1, ty2, ..., tyn) type_name] *)
  | TyParam of ty_param  (** ['a] *)
  | TyArrow of 'a ty * 'a comp_ty  (** [ty1 -> ty2 ! rho] *)
  | TyTuple of 'a ty list  (** [ty1 * ty2 * ... * tyn] *)
  | TyBox of 'a resource_grade * 'a ty  (** [ [rho]ty ] *)
  | TyHandler of 'a comp_ty * 'a comp_ty

and 'a comp_ty = CompTy of 'a ty * 'a resource_grade  (** [ty ! rho] *)

let bool_ty_name = TyName.fresh "bool"
let int_ty_name = TyName.fresh "int"
let unit_ty_name = TyName.fresh "unit"
let string_ty_name = TyName.fresh "string"
let float_ty_name = TyName.fresh "float"
let list_ty_name = TyName.fresh "list"
let empty_ty_name = TyName.fresh "empty"

type variable = Variable.t
type label = Label.t

let nil_label_string = "$nil$"
let nil_label = Label.fresh nil_label_string
let cons_label_string = "$cons$"
let cons_label = Label.fresh cons_label_string

type 'a pattern =
  | PVar of variable
  | PAnnotated of 'a pattern * 'a ty
  | PAs of 'a pattern * variable
  | PTuple of 'a pattern list
  | PVariant of label * 'a pattern option
  | PConst of Const.t
  | PNonbinding

type 'a expression =
  | Var of variable
  | Const of Const.t
  | Annotated of 'a expression * 'a ty
  | Tuple of 'a expression list
  | Variant of label * 'a expression option
  | Lambda of 'a abstraction
  | PureLambda of 'a abstraction
  | RecLambda of variable * 'a abstraction
  | Handler of 'a abstraction * 'a abstraction OpNameMap.t

and 'a computation =
  | Return of 'a expression
  | Do of 'a computation * 'a abstraction
  | Match of 'a expression * 'a abstraction list
  | Apply of 'a expression * 'a expression
  | Delay of int * 'a computation
  | Box of 'a resource_grade * 'a expression * 'a abstraction
  | Unbox of 'a expression * 'a abstraction
  | Perform of operation * 'a expression * 'a abstraction
  | Handle of 'a computation * 'a expression

and 'a abstraction = 'a pattern * 'a computation

type 'a ty_def = TySum of (label * 'a ty option) list | TyInline of 'a ty

type 'a command =
  | TyDef of (ty_param list * ty_name * 'a ty_def) list
  | OpSig of (operation * 'a ty * 'a ty * 'a resource_grade)
  | TopLet of variable * 'a expression
  | TopDo of 'a computation
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
