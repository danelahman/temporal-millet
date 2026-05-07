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

type 'tau ty = 'tau plain_ty annotated

and 'tau plain_ty =
  | TyConst of Language.Const.ty
  | TyApply of ty_name * 'tau ty list  (** [(ty1, ty2, ..., tyn) type_name] *)
  | TyParam of ty_param  (** ['a] *)
  | TyArrow of 'tau ty * 'tau plain_comp_ty  (** [ty1 -> ty2 ! tau] *)
  | TyTuple of 'tau ty list  (** [ty1 * ty2 * ... * tyn] *)
  | TyBox of 'tau * 'tau ty  (** [ [tau]ty ] *)
  | TyHandler of 'tau plain_comp_ty * 'tau plain_comp_ty

and 'tau plain_comp_ty = CompTy of 'tau ty * 'tau  (** [ty ! tau] *)

type variable = string
type label = string
type operation = string

let nil_label = Language.Ast.nil_label_string
let cons_label = Language.Ast.cons_label_string

type 'tau pattern = 'tau plain_pattern annotated

and 'tau plain_pattern =
  | PVar of variable
  | PAnnotated of 'tau pattern * 'tau ty
  | PAs of 'tau pattern * variable
  | PTuple of 'tau pattern list
  | PVariant of label * 'tau pattern option
  | PConst of Language.Const.t
  | PNonbinding

type 'tau term = 'tau plain_term annotated

and 'tau plain_term =
  | Var of variable  (** variables *)
  | Const of Language.Const.t  (** integers, strings, booleans, and floats *)
  | Annotated of 'tau term * 'tau ty
  | Tuple of 'tau term list  (** [(t1, t2, ..., tn)] *)
  | Variant of label * 'tau term option  (** [Label] or [Label t] *)
  | Lambda of 'tau abstraction  (** [fun p1 p2 ... pn -> t] *)
  | PureLambda of 'tau abstraction  (** [fun p1 p2 ... pn -> t] *)
  | Function of 'tau abstraction list
      (** [function p1 -> t1 | ... | pn -> tn] *)
  | Let of 'tau pattern * 'tau term * 'tau term  (** [let p = t1 in t2] *)
  | LetRec of variable * 'tau term * 'tau term  (** [let rec f = t1 in t2] *)
  | Match of 'tau term * 'tau abstraction list
      (** [match t with p1 -> t1 | ... | pn -> tn] *)
  | Conditional of 'tau term * 'tau term * 'tau term
      (** [if t then t1 else t2] *)
  | Apply of 'tau term * 'tau term  (** [t1 t2] *)
  | Delay of int  (** [delay tau] **)
  | Box of 'tau * 'tau term * 'tau abstraction  (** [box tau expr as v in n] *)
  | GenBox of 'tau * 'tau term  (** [box tau expr] *)
  | Unbox of 'tau term * 'tau abstraction  (** [unbox expr as v in n] *)
  | GenUnbox of 'tau term  (** [unbox expr] *)
  | Perform of operation * 'tau term  (** [perform op expr] *)
  | Handler of 'tau abstraction * (operation * 'tau abstraction) list
  | Continue of 'tau term * 'tau term
  | Handle of 'tau term * 'tau term

and 'tau abstraction = 'tau pattern * 'tau term
and 'tau guarded_abstraction = 'tau pattern * 'tau term option * 'tau term

type 'tau ty_def =
  | TySum of (label * 'tau ty option) list
      (** [Label1 of ty1 | Label2 of ty2 | ... | Labeln of tyn | Label' |
           Label''] *)
  | TyInline of 'tau ty  (** [ty] *)

type 'tau command = 'tau plain_command annotated

and 'tau plain_command =
  | TyDef of (ty_param list * ty_name * 'tau ty_def) list
      (** [type ('a...1) t1 = def1 and ... and ('a...n) tn = defn] *)
  | OpSig of (operation * 'tau ty * 'tau ty * 'tau)
      (** [operation op : t1 -> t2 # tau ] *)
  | TopLet of variable * 'tau term  (** [let x = t] *)
  | TopLetRec of variable * 'tau term  (** [let rec f = t] *)
  | TopDo of 'tau term  (** [do t] *)
  | Resources of string  (** [resources "name"] *)
