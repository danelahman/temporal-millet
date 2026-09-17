module Location = Utils.Location
module Symbol = Utils.Symbol
module Variable = Symbol.Make ()
module VariableMap = Map.Make (Variable)
module Label = Symbol.Make ()
module TyName = Symbol.Make ()

type ty_name = TyName.t

module TyNameMap = Map.Make (TyName)
module TyNameSet = Set.Make (TyName)
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
module OpNameSet = Set.Make (OpName)

type operation = OpName.t

type 'a rho =
  | RhoConst of 'a
  | RhoParam of rho_param  (** an unknown grade, solved by unification *)
  | RhoRigid of rho_param
      (** the grade of a handler continuation: universally quantified, so it is
          never substituted and may not occur in the type of a definition *)
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

type rigid_origin = {
  op : operation;
  continuation : variable option;
  case_at : Location.t;
  continuation_at : Location.t;
}
(** Where a rigid continuation grade was introduced, so that a message can name
    the continuation it belongs to. [continuation] is the variable the case
    binds it to, when the pattern is one. *)

(** How a grade came to be accumulated, for the messages that explain why a
    variable may no longer be used. *)
type elapsed_kind =
  | Delayed of int  (** [delay n] *)
  | Performed of operation  (** [perform Op] *)
  | Sequenced  (** the grade of a computation bound by [let] or [;] *)
  | Boxed  (** the value of a [box ρ] is checked ρ ahead *)
  | Handled
      (** the return clause of a handler runs after the handled computation *)

(** The position within a type equation a decomposed equation came from. *)
type step =
  | Argument
  | Result  (** of a function type *)
  | Component of int  (** of a tuple, from 1 *)
  | TypeArgument of int  (** of a type application, from 1 *)
  | BoxContent
  | HandlerInput
  | HandlerOutput

(** Why a constraint was generated. Each constructor carries the places its
    messages point at, so a message cannot ask for one the reason lacks. *)
type 'a why =
  | Application of { func_at : Location.t; arg_at : Location.t }
  | MatchScrutinee of { scrutinee_at : Location.t }  (** at = the pattern *)
  | MatchBranch
      (** at = the branch body; its type and grade must agree with the earlier
          branches *)
  | Annotation  (** at = the annotated expression *)
  | PatternAnnotation
  | VariantArgument of label
  | BoxedValue  (** at = the box *)
  | Unboxed of {
      var : variable;
      bound_at : Location.t option;
      elapsed : ('a rho * Location.t * elapsed_kind) list;
    }
  | UseAfterTime of {
      var : variable;
      bound_at : Location.t;
      elapsed : ('a rho * Location.t * elapsed_kind) list;
    }
  | InstanceOf of {
      var : variable;
      defined_at : Location.t option;
      inner : 'a reason;
    }
  | HandlerCase of { op : operation; signature_at : Location.t }
  | ContinuationGrade of { op : operation; signature_at : Location.t }
      (** the case's grade ≾ the grade of [Op] plus the continuation's *)
  | PerformArgument of { op : operation; signature_at : Location.t }
  | PerformContinuation of { op : operation; signature_at : Location.t }
  | HandleWith  (** at = the handler expression of a [handle] *)
  | RecursiveDefinition of variable
  | PureBody  (** a pure or recursive function body has grade 0 *)
  | Sequencing
      (** [Do]: the bound computation's grade names the context entry *)
  | DefaultOf of { op : operation; signature_at : Location.t }

and 'a reason = { at : Location.t; why : 'a why; path : step list }
(** [at] is the construct the constraint was generated for, [path] the position
    within it a decomposed equation came from, innermost last.

    The elapsed grades are the one thing a reason carries that inference has yet
    to decide; {!substitute_constr} solves them along with the constraint.
    Everything else is plain data — no types, no closures — so that two reasons
    may be compared with polymorphic equality. *)

(** The constraints of a typing derivation besides the equations unification
    solves. Those left over qualify the definition's generalised scheme. *)
type 'a constr =
  | Ineq of 'a rho * 'a rho * 'a reason  (** [rho1] is a sub-grade of [rho2] *)
  | Eternal of 'a ty * 'a reason  (** the type is eternal *)
  | EternalOrIneq of 'a ty * 'a rho * 'a rho * 'a reason
      (** the type is eternal, or [rho1] is a sub-grade of [rho2] *)

type 'a ty_scheme = {
  ty_params : ty_param list;
  rho_params : rho_param list;
  constrs : 'a constr list;
  ty : 'a ty;
}
(** A generalised type. It lives here rather than in the typechecker because
    {!PrettyPrint} prints it and the variable context stores it. *)

(* After [reason], which has an [at] of its own: an unannotated [.at] resolves
   to the last declared, and almost every [.at] wants a syntax node's span. *)
type 'a located = 'a Location.located = { it : 'a; at : Location.t }

let located at it = { it; at }

(* Patterns, expressions and computations carry the span they were desugared
   from; types do not, since unification rebuilds them out of no source. *)
type 'a pattern = 'a plain_pattern located

and 'a plain_pattern =
  | PVar of variable
  | PAnnotated of 'a pattern * 'a ty
  | PAs of 'a pattern * variable
  | PTuple of 'a pattern list
  | PVariant of label * 'a pattern option
  | PConst of Const.t
  | PNonbinding

type 'a expression = 'a plain_expression located

and 'a plain_expression =
  | Var of variable
  | Const of Const.t
  | Annotated of 'a expression * 'a ty
  | Tuple of 'a expression list
  | Variant of label * 'a expression option
  | Lambda of 'a abstraction
  | PureLambda of 'a abstraction
  | RecLambda of variable * 'a abstraction
  | Handler of 'a abstraction * 'a abstraction OpNameMap.t

and 'a computation = 'a plain_computation located

and 'a plain_computation =
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

(* A stable order on expressions built from different constructors.
   [Annotated] is looked through before a rank is ever taken. *)
let expression_rank = function
  | Var _ -> 0
  | Const _ -> 1
  | Annotated _ -> 2
  | Tuple _ -> 3
  | Variant _ -> 4
  | Lambda _ -> 5
  | PureLambda _ -> 6
  | RecLambda _ -> 7
  | Handler _ -> 8

(** Structural comparison of value expressions, ignoring their spans: the
    comparison primitives must not tell two equal constants on different lines
    apart. Functions and handlers, rejected by the caller, are only ranked. *)
let rec compare_expression e1 e2 =
  match (e1.it, e2.it) with
  | Annotated (e1', _), _ -> compare_expression e1' e2
  | _, Annotated (e2', _) -> compare_expression e1 e2'
  | Var x, Var y -> Variable.compare x y
  | Const c1, Const c2 -> Stdlib.compare c1 c2
  | Tuple es1, Tuple es2 -> compare_expressions es1 es2
  | Variant (lbl1, arg1), Variant (lbl2, arg2) -> (
      match Label.compare lbl1 lbl2 with
      | 0 -> (
          match (arg1, arg2) with
          | None, None -> 0
          | None, Some _ -> -1
          | Some _, None -> 1
          | Some arg1, Some arg2 -> compare_expression arg1 arg2)
      | c -> c)
  | plain1, plain2 ->
      Int.compare (expression_rank plain1) (expression_rank plain2)

and compare_expressions es1 es2 =
  match (es1, es2) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | e1 :: es1, e2 :: es2 -> (
      match compare_expression e1 e2 with
      | 0 -> compare_expressions es1 es2
      | c -> c)

type 'a ty_def = TySum of (label * 'a ty option) list | TyInline of 'a ty

(* Whether the eternality of a type definition is computed from its structure,
   as usual ([Derived]), or fixed to non-eternal by a [noneternal type ...]
   declaration ([Noneternal]). *)
type eternality = Derived | Noneternal

type 'a plain_command =
  | TyDef of eternality * (ty_param list * ty_name * 'a ty_def) list
  | OpSig of (operation * 'a ty * 'a ty * 'a rho * (int * int) option)
  | OpDefault of operation * 'a abstraction
  | TopLet of variable * 'a expression
  | TopDo of 'a computation

type 'a command = 'a plain_command located
type ('var, 'map, 'rho) context_elem_ty = VarMap of 'map | Rho of 'rho
type ('var, 'map, 'rho) context = ('var, 'map, 'rho) context_elem_ty list

let rec substitute_rho subst = function
  | (RhoConst _ | RhoRigid _) as rho -> rho
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

(** Elapsed grades are solved like any other, so reasons are substituted into
    too: else a label would report the parameter a [let] contributed rather than
    the grade it stands for, which is often nothing at all. *)
let rec substitute_reason rho_subst reason =
  let elapsed =
    List.map (fun (rho, at, kind) -> (substitute_rho rho_subst rho, at, kind))
  in
  let why =
    match reason.why with
    | Unboxed u -> Unboxed { u with elapsed = elapsed u.elapsed }
    | UseAfterTime u -> UseAfterTime { u with elapsed = elapsed u.elapsed }
    | InstanceOf i ->
        InstanceOf { i with inner = substitute_reason rho_subst i.inner }
    | why -> why
  in
  { reason with why }

let substitute_constr ty_subst rho_subst =
  let reason_of = substitute_reason rho_subst in
  function
  | Ineq (rho1, rho2, reason) ->
      Ineq
        ( substitute_rho rho_subst rho1,
          substitute_rho rho_subst rho2,
          reason_of reason )
  | Eternal (ty, reason) ->
      Eternal (substitute_ty ty_subst rho_subst ty, reason_of reason)
  | EternalOrIneq (ty, rho1, rho2, reason) ->
      EternalOrIneq
        ( substitute_ty ty_subst rho_subst ty,
          substitute_rho rho_subst rho1,
          substitute_rho rho_subst rho2,
          reason_of reason )

(** [wrap_reason f c] rewrites the reason of [c] with [f]: instantiating a
    scheme's qualifier nests the definition's reason inside the use's. *)
let wrap_reason f = function
  | Ineq (rho1, rho2, reason) -> Ineq (rho1, rho2, f reason)
  | Eternal (ty, reason) -> Eternal (ty, f reason)
  | EternalOrIneq (ty, rho1, rho2, reason) ->
      EternalOrIneq (ty, rho1, rho2, f reason)

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
  | RhoConst _ | RhoRigid _ -> RhoParamSet.empty
  | RhoParam a -> RhoParamSet.singleton a
  | RhoAdd (l, r) -> RhoParamSet.union (free_rhos l) (free_rhos r)

(** The rigid grades of a grade or a type. They are never substituted or
    generalised, so [free_vars] leaves them out. *)
let rec rigid_rhos = function
  | RhoConst _ | RhoParam _ -> RhoParamSet.empty
  | RhoRigid a -> RhoParamSet.singleton a
  | RhoAdd (l, r) -> RhoParamSet.union (rigid_rhos l) (rigid_rhos r)

(** [instantiate_rigid w rho] takes the instance of [rho] in which every rigid
    grade is [w]. A failing ground instance refutes the universal statement. *)
let rec instantiate_rigid w = function
  | RhoRigid _ -> RhoConst w
  | RhoAdd (l, r) -> RhoAdd (instantiate_rigid w l, instantiate_rigid w r)
  | rho -> rho

let rec rigid_rhos_ty = function
  | TyConst _ | TyParam _ -> RhoParamSet.empty
  | TyApply (_, tys) | TyTuple tys ->
      List.fold_left
        (fun acc ty -> RhoParamSet.union acc (rigid_rhos_ty ty))
        RhoParamSet.empty tys
  | TyArrow (ty1, CompTy (ty2, rho)) ->
      RhoParamSet.union
        (RhoParamSet.union (rigid_rhos_ty ty1) (rigid_rhos_ty ty2))
        (rigid_rhos rho)
  | TyBox (rho, ty) -> RhoParamSet.union (rigid_rhos rho) (rigid_rhos_ty ty)
  | TyHandler (CompTy (ty1, rho1), CompTy (ty2, rho2)) ->
      RhoParamSet.union
        (RhoParamSet.union (rigid_rhos_ty ty1) (rigid_rhos_ty ty2))
        (RhoParamSet.union (rigid_rhos rho1) (rigid_rhos rho2))

let rigid_rhos_comp_ty = function
  | CompTy (ty, rho) -> RhoParamSet.union (rigid_rhos_ty ty) (rigid_rhos rho)

(* The reasons are not looked at: their only grades are the elapsed entries,
   the summands of a grade the constraint already states. *)
let free_vars_constr = function
  | Ineq (rho1, rho2, _) ->
      (TyParamSet.empty, RhoParamSet.union (free_rhos rho1) (free_rhos rho2))
  | Eternal (ty, _) -> free_vars ty
  | EternalOrIneq (ty, rho1, rho2, _) ->
      let fv_ty, fv_rho = free_vars ty in
      ( fv_ty,
        RhoParamSet.union fv_rho
          (RhoParamSet.union (free_rhos rho1) (free_rhos rho2)) )
