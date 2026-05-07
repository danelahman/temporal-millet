module Location = Utils.Location

type ty_name = string

let bool_ty_name = "bool"
let int_ty_name = "int"
let unit_ty_name = "unit"
let string_ty_name = "string"
let float_ty_name = "float"
let list_ty_name = "list"
let empty_ty_name = "empty"

type 'a annotated = { it : 'a; at : Location.t }
type ty_param = string

type 'rho ty = 'rho plain_ty annotated

and 'rho plain_ty =
  | TyConst of Language.Const.ty
  | TyApply of ty_name * 'rho ty list  (** [(ty1, ty2, ..., tyn) type_name] *)
  | TyParam of ty_param  (** ['a] *)
  | TyArrow of 'rho ty * 'rho plain_comp_ty  (** [ty1 -> ty2 ! rho] *)
  | TyTuple of 'rho ty list  (** [ty1 * ty2 * ... * tyn] *)
  | TyBox of 'rho * 'rho ty  (** [ [rho]ty ] *)
  | TyHandler of 'rho plain_comp_ty * 'rho plain_comp_ty

and 'rho plain_comp_ty = CompTy of 'rho ty * 'rho  (** [ty ! rho] *)

type variable = string
type label = string
type operation = string

let nil_label = Language.Ast.nil_label_string
let cons_label = Language.Ast.cons_label_string

type 'rho pattern = 'rho plain_pattern annotated

and 'rho plain_pattern =
  | PVar of variable
  | PAnnotated of 'rho pattern * 'rho ty
  | PAs of 'rho pattern * variable
  | PTuple of 'rho pattern list
  | PVariant of label * 'rho pattern option
  | PConst of Language.Const.t
  | PNonbinding

type 'rho term = 'rho plain_term annotated

and 'rho plain_term =
  | Var of variable  (** variables *)
  | Const of Language.Const.t  (** integers, strings, booleans, and floats *)
  | Annotated of 'rho term * 'rho ty
  | Tuple of 'rho term list  (** [(t1, t2, ..., tn)] *)
  | Variant of label * 'rho term option  (** [Label] or [Label t] *)
  | Lambda of 'rho abstraction  (** [fun p1 p2 ... pn -> t] *)
  | PureLambda of 'rho abstraction  (** [fun p1 p2 ... pn -> t] *)
  | Function of 'rho abstraction list
      (** [function p1 -> t1 | ... | pn -> tn] *)
  | Let of 'rho pattern * 'rho term * 'rho term  (** [let p = t1 in t2] *)
  | LetRec of variable * 'rho term * 'rho term  (** [let rec f = t1 in t2] *)
  | Match of 'rho term * 'rho abstraction list
      (** [match t with p1 -> t1 | ... | pn -> tn] *)
  | Conditional of 'rho term * 'rho term * 'rho term
      (** [if t then t1 else t2] *)
  | Apply of 'rho term * 'rho term  (** [t1 t2] *)
  | Delay of int  (** [delay rho] **)
  | Box of 'rho * 'rho term * 'rho abstraction  (** [box rho expr as v in n] *)
  | GenBox of 'rho * 'rho term  (** [box rho expr] *)
  | Unbox of 'rho term * 'rho abstraction  (** [unbox expr as v in n] *)
  | GenUnbox of 'rho term  (** [unbox expr] *)
  | Perform of operation * 'rho term  (** [perform op expr] *)
  | Handler of 'rho abstraction * (operation * 'rho abstraction) list
  | Continue of 'rho term * 'rho term
  | Handle of 'rho term * 'rho term

and 'rho abstraction = 'rho pattern * 'rho term
and 'rho guarded_abstraction = 'rho pattern * 'rho term option * 'rho term

type 'rho ty_def =
  | TySum of (label * 'rho ty option) list
      (** [Label1 of ty1 | Label2 of ty2 | ... | Labeln of tyn | Label' |
           Label''] *)
  | TyInline of 'rho ty  (** [ty] *)

type 'rho command = 'rho plain_command annotated

and 'rho plain_command =
  | TyDef of (ty_param list * ty_name * 'rho ty_def) list
      (** [type ('a...1) t1 = def1 and ... and ('a...n) tn = defn] *)
  | OpSig of (operation * 'rho ty * 'rho ty * 'rho)
      (** [operation op : t1 -> t2 # rho ] *)
  | TopLet of variable * 'rho term  (** [let x = t] *)
  | TopLetRec of variable * 'rho term  (** [let rec f = t] *)
  | TopDo of 'rho term  (** [do t] *)
  | Resources of string  (** [resources "name"] *)
