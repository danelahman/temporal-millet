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

module TauParamModule = Symbol.Make ()
module TauParamMap = Map.Make (TauParamModule)
module TauParamSet = Set.Make (TauParamModule)

type tau_param = TauParamModule.t

module OpName = Symbol.Make ()
module OpNameMap = Map.Make (OpName)

type operation = OpName.t

type 'a tau =
  | TauConst of 'a
  | TauParam of tau_param
  | TauAdd of 'a tau * 'a tau
  | TauMeet of 'a tau * 'a tau

type 'a ty =
  | TyConst of Const.ty
  | TyApply of ty_name * 'a ty list  (** [(ty1, ty2, ..., tyn) type_name] *)
  | TyParam of ty_param  (** ['a] *)
  | TyArrow of 'a ty * 'a comp_ty  (** [ty1 -> ty2 ! tau] *)
  | TyTuple of 'a ty list  (** [ty1 * ty2 * ... * tyn] *)
  | TyBox of 'a tau * 'a ty  (** [ [tau]ty ] *)
  | TyHandler of 'a comp_ty * 'a comp_ty

and 'a comp_ty = CompTy of 'a ty * 'a tau  (** [ty ! tau] *)

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
  | Delay of 'a tau * 'a computation
  | Box of 'a tau * 'a expression * 'a abstraction
  | Unbox of 'a expression * 'a abstraction
  | Perform of operation * 'a expression * 'a abstraction
  | Handle of 'a computation * 'a expression

and 'a abstraction = 'a pattern * 'a computation

type 'a ty_def = TySum of (label * 'a ty option) list | TyInline of 'a ty

type 'a command =
  | TyDef of (ty_param list * ty_name * 'a ty_def) list
  | OpSig of (operation * 'a ty * 'a ty * 'a tau)
  | TopLet of variable * 'a expression
  | TopDo of 'a computation

type ('var, 'map, 'tau) context_elem_ty = VarMap of 'map | Tau of 'tau
type ('var, 'map, 'tau) context = ('var, 'map, 'tau) context_elem_ty list

let rec substitute_tau subst = function
  | TauConst _ as tau -> tau
  | TauParam tp as tau -> (
      match TauParamMap.find_opt tp subst with None -> tau | Some tau' -> tau')
  | TauAdd (tau, tau') ->
      TauAdd (substitute_tau subst tau, substitute_tau subst tau')
  | TauMeet (tau, tau') ->
      TauMeet (substitute_tau subst tau, substitute_tau subst tau')

let rec substitute_ty ty_subst tau_subst = function
  | TyConst _ as ty -> ty
  | TyParam a as ty -> (
      match TyParamMap.find_opt a ty_subst with None -> ty | Some ty' -> ty')
  | TyApply (ty_name, tys) ->
      TyApply (ty_name, List.map (substitute_ty ty_subst tau_subst) tys)
  | TyTuple tys -> TyTuple (List.map (substitute_ty ty_subst tau_subst) tys)
  | TyArrow (ty1, CompTy (ty2, tau)) ->
      TyArrow
        ( substitute_ty ty_subst tau_subst ty1,
          CompTy
            (substitute_ty ty_subst tau_subst ty2, substitute_tau tau_subst tau)
        )
  | TyBox (tau, ty) ->
      TyBox (substitute_tau tau_subst tau, substitute_ty ty_subst tau_subst ty)
  | TyHandler (CompTy (ty1, tau1), CompTy (ty2, tau2)) ->
      TyHandler
        ( CompTy
            (substitute_ty ty_subst tau_subst ty1, substitute_tau tau_subst tau1),
          CompTy
            (substitute_ty ty_subst tau_subst ty2, substitute_tau tau_subst tau2)
        )

let substitute_comp_ty ty_subst tau_subst = function
  | CompTy (ty, tau) ->
      CompTy (substitute_ty ty_subst tau_subst ty, substitute_tau tau_subst tau)

let rec free_vars = function
  | TyConst _ -> (TyParamSet.empty, TauParamSet.empty)
  | TyParam a -> (TyParamSet.singleton a, TauParamSet.empty)
  | TyApply (_, tys) ->
      List.fold_left
        (fun (ty_params, tau_params) ty ->
          let fv_ty, fv_tau = free_vars ty in
          (TyParamSet.union ty_params fv_ty, TauParamSet.union tau_params fv_tau))
        (TyParamSet.empty, TauParamSet.empty)
        tys
  | TyTuple tys ->
      List.fold_left
        (fun (ty_params, tau_params) ty ->
          let fv_ty, fv_tau = free_vars ty in
          (TyParamSet.union ty_params fv_ty, TauParamSet.union tau_params fv_tau))
        (TyParamSet.empty, TauParamSet.empty)
        tys
  | TyArrow (ty1, CompTy (ty2, tau)) ->
      let fv_ty1, fv_tau1 = free_vars ty1 in
      let fv_ty2, fv_tau2 = free_vars ty2 in
      let nested_free_taus = free_taus tau in
      ( TyParamSet.union fv_ty1 fv_ty2,
        TauParamSet.union (TauParamSet.union fv_tau1 fv_tau2) nested_free_taus
      )
  | TyBox (tau, ty) ->
      let fv_ty, fv_tau = free_vars ty in
      let nested_free_taus = free_taus tau in
      (fv_ty, TauParamSet.union fv_tau nested_free_taus)
  | TyHandler (CompTy (ty1, tau1), CompTy (ty2, tau2)) ->
      let fv_ty1, fv_tau1 = free_vars ty1 in
      let fv_ty2, fv_tau2 = free_vars ty2 in
      let nested_free_taus1 = free_taus tau1 in
      let nested_free_taus2 = free_taus tau2 in
      ( TyParamSet.union fv_ty1 fv_ty2,
        TauParamSet.union
          (TauParamSet.union fv_tau1 fv_tau2)
          (TauParamSet.union nested_free_taus1 nested_free_taus2) )

and free_taus tau =
  match tau with
  | TauConst _ -> TauParamSet.empty
  | TauParam a -> TauParamSet.singleton a
  | TauAdd (l, r) -> TauParamSet.union (free_taus l) (free_taus r)
  | TauMeet (l, r) -> TauParamSet.union (free_taus l) (free_taus r)
