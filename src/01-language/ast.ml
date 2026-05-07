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

module RhoParamModule = Symbol.Make ()
module RhoParamMap = Map.Make (RhoParamModule)
module RhoParamSet = Set.Make (RhoParamModule)

type rho_param = RhoParamModule.t

module OpName = Symbol.Make ()
module OpNameMap = Map.Make (OpName)

type operation = OpName.t

type 'a rho =
  | RhoConst of 'a
  | RhoParam of rho_param
  | RhoAdd of 'a rho * 'a rho

type 'a ty =
  | TyConst of Const.ty
  | TyApply of ty_name * 'a ty list  (** [(ty1, ty2, ..., tyn) type_name] *)
  | TyParam of ty_param  (** ['a] *)
  | TyArrow of 'a ty * 'a comp_ty  (** [ty1 -> ty2 ! rho] *)
  | TyTuple of 'a ty list  (** [ty1 * ty2 * ... * tyn] *)
  | TyBox of 'a rho * 'a ty  (** [ [rho]ty ] *)
  | TyHandler of 'a comp_ty * 'a comp_ty

and 'a comp_ty = CompTy of 'a ty * 'a rho  (** [ty ! rho] *)

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
  | Box of 'a rho * 'a expression * 'a abstraction
  | Unbox of 'a expression * 'a abstraction
  | Perform of operation * 'a expression * 'a abstraction
  | Handle of 'a computation * 'a expression

and 'a abstraction = 'a pattern * 'a computation

type 'a ty_def = TySum of (label * 'a ty option) list | TyInline of 'a ty

type 'a command =
  | TyDef of (ty_param list * ty_name * 'a ty_def) list
  | OpSig of (operation * 'a ty * 'a ty * 'a rho)
  | TopLet of variable * 'a expression
  | TopDo of 'a computation
  | Resources of string

type ('var, 'map, 'rho) context_elem_ty = VarMap of 'map | Rho of 'rho
type ('var, 'map, 'rho) context = ('var, 'map, 'rho) context_elem_ty list

let rec substitute_rho subst = function
  | RhoConst _ as rho -> rho
  | RhoParam tp as rho -> (
      match RhoParamMap.find_opt tp subst with None -> rho | Some rho' -> rho')
  | RhoAdd (rho, rho') ->
      RhoAdd (substitute_rho subst rho, substitute_rho subst rho')

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
            (substitute_ty ty_subst rho_subst ty2, substitute_rho rho_subst rho)
        )
  | TyBox (rho, ty) ->
      TyBox (substitute_rho rho_subst rho, substitute_ty ty_subst rho_subst ty)
  | TyHandler (CompTy (ty1, rho1), CompTy (ty2, rho2)) ->
      TyHandler
        ( CompTy
            (substitute_ty ty_subst rho_subst ty1, substitute_rho rho_subst rho1),
          CompTy
            (substitute_ty ty_subst rho_subst ty2, substitute_rho rho_subst rho2)
        )

let substitute_comp_ty ty_subst rho_subst = function
  | CompTy (ty, rho) ->
      CompTy (substitute_ty ty_subst rho_subst ty, substitute_rho rho_subst rho)

let rec free_vars = function
  | TyConst _ -> (TyParamSet.empty, RhoParamSet.empty)
  | TyParam a -> (TyParamSet.singleton a, RhoParamSet.empty)
  | TyApply (_, tys) ->
      List.fold_left
        (fun (ty_params, rho_params) ty ->
          let fv_ty, fv_rho = free_vars ty in
          (TyParamSet.union ty_params fv_ty, RhoParamSet.union rho_params fv_rho))
        (TyParamSet.empty, RhoParamSet.empty)
        tys
  | TyTuple tys ->
      List.fold_left
        (fun (ty_params, rho_params) ty ->
          let fv_ty, fv_rho = free_vars ty in
          (TyParamSet.union ty_params fv_ty, RhoParamSet.union rho_params fv_rho))
        (TyParamSet.empty, RhoParamSet.empty)
        tys
  | TyArrow (ty1, CompTy (ty2, rho)) ->
      let fv_ty1, fv_rho1 = free_vars ty1 in
      let fv_ty2, fv_rho2 = free_vars ty2 in
      let nested_free_rhos = free_rhos rho in
      ( TyParamSet.union fv_ty1 fv_ty2,
        RhoParamSet.union (RhoParamSet.union fv_rho1 fv_rho2) nested_free_rhos
      )
  | TyBox (rho, ty) ->
      let fv_ty, fv_rho = free_vars ty in
      let nested_free_rhos = free_rhos rho in
      (fv_ty, RhoParamSet.union fv_rho nested_free_rhos)
  | TyHandler (CompTy (ty1, rho1), CompTy (ty2, rho2)) ->
      let fv_ty1, fv_rho1 = free_vars ty1 in
      let fv_ty2, fv_rho2 = free_vars ty2 in
      let nested_free_rhos1 = free_rhos rho1 in
      let nested_free_rhos2 = free_rhos rho2 in
      ( TyParamSet.union fv_ty1 fv_ty2,
        RhoParamSet.union
          (RhoParamSet.union fv_rho1 fv_rho2)
          (RhoParamSet.union nested_free_rhos1 nested_free_rhos2) )

and free_rhos rho =
  match rho with
  | RhoConst _ -> RhoParamSet.empty
  | RhoParam a -> RhoParamSet.singleton a
  | RhoAdd (l, r) -> RhoParamSet.union (free_rhos l) (free_rhos r)
