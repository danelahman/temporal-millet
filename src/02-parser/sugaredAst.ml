module Location = Utils.Location

type ty_name = string
type tau_val = int

let bool_ty_name = "bool"
let int_ty_name = "int"
let unit_ty_name = "unit"
let string_ty_name = "string"
let float_ty_name = "float"
let list_ty_name = "list"
let empty_ty_name = "empty"

type 'a annotated = { it : 'a; at : Location.t }
type ty_param = string

type ty = plain_ty annotated

and plain_ty =
  | TyConst of Language.Const.ty
  | TyApply of ty_name * ty list  (** [(ty1, ty2, ..., tyn) type_name] *)
  | TyParam of ty_param  (** ['a] *)
  | TyArrow of ty * plain_comp_ty  (** [ty1 -> ty2 ! tau] *)
  | TyTuple of ty list  (** [ty1 * ty2 * ... * tyn] *)
  | TyBox of tau_val * ty  (** [ [tau]ty ] *)
  | TyHandler of plain_comp_ty * plain_comp_ty

and plain_comp_ty = CompTy of ty * tau_val  (** [ty ! tau] *)

type variable = string
type label = string
type operation = string

let nil_label = Language.Ast.nil_label_string
let cons_label = Language.Ast.cons_label_string

type pattern = plain_pattern annotated

and plain_pattern =
  | PVar of variable
  | PAnnotated of pattern * ty
  | PAs of pattern * variable
  | PTuple of pattern list
  | PVariant of label * pattern option
  | PConst of Language.Const.t
  | PNonbinding

type term = plain_term annotated

and plain_term =
  | Var of variable  (** variables *)
  | Const of Language.Const.t  (** integers, strings, booleans, and floats *)
  | Annotated of term * ty
  | Tuple of term list  (** [(t1, t2, ..., tn)] *)
  | Variant of label * term option  (** [Label] or [Label t] *)
  | Lambda of abstraction  (** [fun p1 p2 ... pn -> t] *)
  | PureLambda of abstraction  (** [fun p1 p2 ... pn -> t] *)
  | Function of abstraction list  (** [function p1 -> t1 | ... | pn -> tn] *)
  | Let of pattern * term * term  (** [let p = t1 in t2] *)
  | LetRec of variable * term * term  (** [let rec f = t1 in t2] *)
  | Match of term * abstraction list
      (** [match t with p1 -> t1 | ... | pn -> tn] *)
  | Conditional of term * term * term  (** [if t then t1 else t2] *)
  | Apply of term * term  (** [t1 t2] *)
  | Delay of int  (** [delay tau] **)
  | Box of int * term * abstraction  (** [box tau expr as v in n] *)
  | GenBox of int * term  (** [box tau expr] *)
  | Unbox of int * term * abstraction  (** [unbox tau expr as v in n] *)
  | GenUnbox of int * term  (** [unbox tau expr] *)
  | Perform of operation * term  (** [perform op expr] *)
  | Handler of abstraction * (operation * abstraction) list

and abstraction = pattern * term
and guarded_abstraction = pattern * term option * term

type ty_def =
  | TySum of (label * ty option) list
      (** [Label1 of ty1 | Label2 of ty2 | ... | Labeln of tyn | Label' |
           Label''] *)
  | TyInline of ty  (** [ty] *)

type command = plain_command annotated

and plain_command =
  | TyDef of (ty_param list * ty_name * ty_def) list
      (** [type ('a...1) t1 = def1 and ... and ('a...n) tn = defn] *)
  | OpSig of (operation * ty * ty * tau_val)
      (** [operation op : t1 -> t2 # tau ] *)
  | TopLet of variable * term  (** [let x = t] *)
  | TopLetRec of variable * term  (** [let rec f = t] *)
  | TopDo of term  (** [do t] *)
