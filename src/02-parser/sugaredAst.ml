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

type 'resource_grade ty = 'resource_grade plain_ty annotated

and 'resource_grade plain_ty =
  | TyConst of Language.Const.ty
  | TyApply of ty_name * 'resource_grade ty list
      (** [(ty1, ty2, ..., tyn) type_name] *)
  | TyParam of ty_param  (** ['a] *)
  | TyArrow of 'resource_grade ty * 'resource_grade plain_comp_ty
      (** [ty1 -> ty2 ! rho] *)
  | TyTuple of 'resource_grade ty list  (** [ty1 * ty2 * ... * tyn] *)
  | TyBox of 'resource_grade * 'resource_grade ty  (** [ [rho]ty ] *)
  | TyHandler of 'resource_grade plain_comp_ty * 'resource_grade plain_comp_ty

and 'resource_grade plain_comp_ty =
  | CompTy of 'resource_grade ty * 'resource_grade  (** [ty ! rho] *)

type variable = string
type label = string
type operation = string

let nil_label = Language.Ast.nil_label_string
let cons_label = Language.Ast.cons_label_string

type 'resource_grade pattern = 'resource_grade plain_pattern annotated

and 'resource_grade plain_pattern =
  | PVar of variable
  | PAnnotated of 'resource_grade pattern * 'resource_grade ty
  | PAs of 'resource_grade pattern * variable
  | PTuple of 'resource_grade pattern list
  | PVariant of label * 'resource_grade pattern option
  | PConst of Language.Const.t
  | PNonbinding

type 'resource_grade term = 'resource_grade plain_term annotated

and 'resource_grade plain_term =
  | Var of variable  (** variables *)
  | Const of Language.Const.t  (** integers, strings, booleans, and floats *)
  | Annotated of 'resource_grade term * 'resource_grade ty
  | Tuple of 'resource_grade term list  (** [(t1, t2, ..., tn)] *)
  | Variant of label * 'resource_grade term option  (** [Label] or [Label t] *)
  | Lambda of 'resource_grade abstraction  (** [fun p1 p2 ... pn -> t] *)
  | PureLambda of 'resource_grade abstraction  (** [fun p1 p2 ... pn -> t] *)
  | Function of 'resource_grade abstraction list
      (** [function p1 -> t1 | ... | pn -> tn] *)
  | Let of 'resource_grade pattern * 'resource_grade term * 'resource_grade term
      (** [let p = t1 in t2] *)
  | LetRec of variable * 'resource_grade term * 'resource_grade term
      (** [let rec f = t1 in t2] *)
  | Match of 'resource_grade term * 'resource_grade abstraction list
      (** [match t with p1 -> t1 | ... | pn -> tn] *)
  | Conditional of
      'resource_grade term * 'resource_grade term * 'resource_grade term
      (** [if t then t1 else t2] *)
  | Apply of 'resource_grade term * 'resource_grade term  (** [t1 t2] *)
  | Delay of int  (** [delay rho] **)
  | Box of 'resource_grade * 'resource_grade term * 'resource_grade abstraction
      (** [box rho expr as v in n] *)
  | GenBox of 'resource_grade * 'resource_grade term  (** [box rho expr] *)
  | Unbox of 'resource_grade term * 'resource_grade abstraction
      (** [unbox expr as v in n] *)
  | GenUnbox of 'resource_grade term  (** [unbox expr] *)
  | Perform of operation * 'resource_grade term  (** [perform op expr] *)
  | Handler of
      'resource_grade abstraction
      * (operation * 'resource_grade abstraction) list
  | Continue of 'resource_grade term * 'resource_grade term
  | Handle of 'resource_grade term * 'resource_grade term

and 'resource_grade abstraction = 'resource_grade pattern * 'resource_grade term

and 'resource_grade guarded_abstraction =
  'resource_grade pattern * 'resource_grade term option * 'resource_grade term

type 'resource_grade ty_def =
  | TySum of (label * 'resource_grade ty option) list
      (** [Label1 of ty1 | Label2 of ty2 | ... | Labeln of tyn | Label' |
           Label''] *)
  | TyInline of 'resource_grade ty  (** [ty] *)

type 'resource_grade command = 'resource_grade plain_command annotated

and 'resource_grade plain_command =
  | TyDef of (ty_param list * ty_name * 'resource_grade ty_def) list
      (** [type ('a...1) t1 = def1 and ... and ('a...n) tn = defn] *)
  | OpSig of
      (operation * 'resource_grade ty * 'resource_grade ty * 'resource_grade)
      (** [operation op : t1 -> t2 # rho ] *)
  | TopLet of variable * 'resource_grade term  (** [let x = t] *)
  | TopLetRec of variable * 'resource_grade term  (** [let rec f = t] *)
  | TopDo of 'resource_grade term  (** [do t] *)
  | Grades of string  (** [grades "name"] *)
