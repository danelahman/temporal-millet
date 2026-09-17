open Ast
module Print = Utils.Print
module Symbol = Utils.Symbol

(* Unicode subscripts for digits 0–9 *)
let subscript i =
  let digits =
    [|
      "\226\130\128";
      "\226\130\129";
      "\226\130\130";
      "\226\130\131";
      "\226\130\132";
      "\226\130\133";
      "\226\130\134";
      "\226\130\135";
      "\226\130\136";
      "\226\130\137";
    |]
  in
  let rec build_subscript n =
    if n < 10 then digits.(n) else build_subscript (n / 10) ^ digits.(n mod 10)
  in
  build_subscript i

(* Greek letters for type variables *)
let greek_letters =
  [|
    "α";
    "β";
    "γ";
    "δ";
    "ε";
    "ζ";
    "η";
    "θ";
    "ι";
    "κ";
    "λ";
    "μ";
    "ν";
    "ξ";
    "ο";
    "π";
    "σ";
    "τ";
  |]

let type_symbol n =
  if n < Array.length greek_letters then greek_letters.(n)
  else "σ" ^ subscript (n - Array.length greek_letters)

let rho_symbol n = "ρ" ^ subscript n

module MakeParamPrinter
    (ParamMap : Map.S)
    (SymbolGen : sig
      val symbol_for_index : int -> string
    end) =
struct
  let create () =
    let names = ref ParamMap.empty in
    let counter = ref 0 in
    fun param ppf ->
      let symbol =
        match ParamMap.find_opt param !names with
        | Some sym -> sym
        | None ->
            let sym = SymbolGen.symbol_for_index !counter in
            incr counter;
            names := ParamMap.add param sym !names;
            sym
      in
      Format.fprintf ppf "%s" symbol
end

module TyPrintParam =
  MakeParamPrinter
    (TyParamMap)
    (struct
      let symbol_for_index = type_symbol
    end)

module RhoPrintParam =
  MakeParamPrinter
    (RhoParamMap)
    (struct
      let symbol_for_index = rho_symbol
    end)

let print_ty_params ty_pp ty_params ppf =
  Format.fprintf ppf "[";
  let rec print_helper = function
    | [] -> ()
    | [ last ] -> Format.fprintf ppf "%t" (ty_pp last)
    | hd :: tl ->
        Format.fprintf ppf "%t, " (ty_pp hd);
        print_helper tl
  in
  print_helper ty_params;
  Format.fprintf ppf "]"

let print_rho_params ?max_level:_ rho_pp rho_params ppf =
  Format.fprintf ppf "[";
  let rec print_helper = function
    | [] -> ()
    | [ last ] -> Format.fprintf ppf "%t" (rho_pp last)
    | hd :: tl ->
        Format.fprintf ppf "%t, " (rho_pp hd);
        print_helper tl
  in
  print_helper rho_params;
  Format.fprintf ppf "]"

let print_rho (type a)
    (module ResourceGrade : ResourceGrade.Grade with type t = a) rho_pp =
  let rec aux (rho : a rho) ppf =
    match rho with
    | RhoConst i -> Format.fprintf ppf "%s" (ResourceGrade.show i)
    | RhoParam p | RhoRigid p -> rho_pp p ppf
    | RhoAdd (t1, t2) ->
        Format.fprintf ppf "@[%t + %t@]"
          (fun ppf -> aux t1 ppf)
          (fun ppf -> aux t2 ppf)
  in
  aux

let print_ty (type a) ?max_level rho_module ty_print_param rho_print_param =
  let module ResourceGrade =
    (val rho_module : ResourceGrade.Grade with type t = a)
  in
  let rec aux ?max_level p ppf =
    let print ?at_level = Print.print ?max_level ?at_level ppf in
    match p with
    | TyConst c -> print "%t" (Const.print_ty c)
    | TyApply (ty_name, []) -> print "%t" (TyName.print ty_name)
    | TyApply (ty_name, [ ty ]) ->
        print ~at_level:1 "%t %t" (aux ~max_level:1 ty) (TyName.print ty_name)
    | TyApply (ty_name, tys) ->
        print ~at_level:1 "%t %t"
          (Print.print_tuple aux tys)
          (TyName.print ty_name)
    | TyParam a -> print "%t" (ty_print_param a)
    | TyArrow (ty1, CompTy (ty2, RhoConst z)) when z = ResourceGrade.zero ->
        print ~at_level:3 "%t → %t" (aux ~max_level:2 ty1)
          (aux ~max_level:3 ty2)
    | TyArrow (ty1, CompTy (ty2, rho)) ->
        print ~at_level:3 "%t → %t # %t" (aux ~max_level:2 ty1)
          (aux ~max_level:3 ty2)
          (print_rho rho_module rho_print_param rho)
    | TyTuple [] -> print "unit"
    | TyTuple tys ->
        print ~at_level:2 "%t"
          (Print.print_sequence " × " (aux ~max_level:1) tys)
    | TyBox (rho, ty) ->
        print ~at_level:1 "[%t]%t"
          (print_rho rho_module rho_print_param rho)
          (aux ~max_level:0 ty)
    | TyHandler (CompTy (ty1, rho1), CompTy (ty2, rho2)) ->
        print ~at_level:3 "%t # %t ⇒ %t # %t" (aux ~max_level:2 ty1)
          (print_rho rho_module rho_print_param rho1)
          (aux ~max_level:3 ty2)
          (print_rho rho_module rho_print_param rho2)
  in
  aux ?max_level

let print_constr (type a) rho_module ty_pp rho_pp =
  let module ResourceGrade =
    (val rho_module : ResourceGrade.Grade with type t = a)
  in
  let print_ineq rho1 rho2 ppf =
    Format.fprintf ppf "%t %s %t"
      (print_rho rho_module rho_pp rho1)
      ResourceGrade.is_sub_rho_symbol
      (print_rho rho_module rho_pp rho2)
  in
  let print_eternal ty ppf =
    Format.fprintf ppf "eternal %t"
      (print_ty ~max_level:0 rho_module ty_pp rho_pp ty)
  in
  (* A constraint's reason is provenance for diagnostics, not part of what the
     constraint says, so a scheme's qualifier prints without it. *)
  fun (c : a constr) ppf ->
    match c with
    | Ineq (rho1, rho2, _) -> print_ineq rho1 rho2 ppf
    | Eternal (ty, _) -> print_eternal ty ppf
    | EternalOrIneq (ty, rho1, rho2, _) ->
        Format.fprintf ppf "%t ∨ %t" (print_eternal ty) (print_ineq rho1 rho2)

(** The qualifier of a type scheme, [{c1, ..., cn}], or nothing when there are
    no constraints. *)
let print_constrs rho_module ty_pp rho_pp constrs ppf =
  match constrs with
  | [] -> ()
  | _ ->
      Format.fprintf ppf "{%t} "
        (Print.print_sequence ", "
           (print_constr rho_module ty_pp rho_pp)
           constrs)

let rec print_pattern ?max_level p ppf =
  let print ?at_level = Print.print ?max_level ?at_level ppf in
  match p.it with
  | PVar x -> print "%t" (Variable.print x)
  | PAs (p, x) -> print "%t as %t" (print_pattern p) (Variable.print x)
  | PAnnotated (p, _ty) -> print_pattern ?max_level p ppf
  | PConst c -> Const.print c ppf
  | PTuple lst -> Print.print_tuple print_pattern lst ppf
  | PVariant (lbl, None) when lbl = nil_label -> print "[]"
  | PVariant (lbl, None) -> print "%t" (Label.print lbl)
  | PVariant (lbl, Some { it = PTuple [ v1; v2 ]; _ }) when lbl = cons_label ->
      print "%t::%t" (print_pattern v1) (print_pattern v2)
  | PVariant (lbl, Some p) ->
      print ~at_level:1 "%t @[<hov>%t@]" (Label.print lbl) (print_pattern p)
  | PNonbinding -> print "_"

and print_expression rho_module =
  let rec aux ?max_level e ppf =
    let print ?at_level = Print.print ?max_level ?at_level ppf in
    match e.it with
    | Var x -> print "%t" (Variable.print x)
    | Const c -> print "%t" (Const.print c)
    | Annotated (t, _ty) -> aux ?max_level t ppf
    | Tuple lst ->
        Print.print_tuple (fun ?max_level e ppf -> aux ?max_level e ppf) lst ppf
    | Variant (lbl, None) when lbl = nil_label -> print "[]"
    | Variant (lbl, None) -> print "%t" (Label.print lbl)
    | Variant (lbl, Some { it = Tuple [ v1; v2 ]; _ }) when lbl = cons_label ->
        print ~at_level:1 "%t::%t" (aux ~max_level:0 v1) (aux ~max_level:1 v2)
    | Variant (lbl, Some arg) ->
        print ~at_level:1 "%t %t" (Label.print lbl) (aux ~max_level:0 arg)
    | Lambda (p, c) ->
        print ~at_level:2 "@[<hv 2>fun %t ↦@ %t@]" (print_pattern p)
          (print_computation rho_module ?max_level:None c)
    | PureLambda (p, c) ->
        print ~at_level:2 "@[<hv 2>fun %t ↦@ %t@]" (print_pattern p)
          (print_computation rho_module ?max_level:None c)
    | RecLambda (f, _ty) -> print ~at_level:2 "rec %t ..." (Variable.print f)
    | Handler (ret_case, op_cases) ->
        let print_op_cases ppf =
          List.iter
            (fun op_case ->
              Format.fprintf ppf "@,| %t" (print_op_case rho_module op_case))
            (OpNameMap.bindings op_cases)
        in
        print "@[<v 0>handler@,| return %t%t@]"
          (print_abstraction rho_module ret_case)
          print_op_cases
  in
  aux

and print_computation rho_module =
  let rec aux ?max_level c ppf =
    let print ?at_level = Print.print ?max_level ?at_level ppf in
    match c.it with
    | Return e ->
        print ~at_level:1 "return %t"
          (print_expression rho_module ~max_level:0 e)
    | Do (c1, ({ it = PNonbinding; _ }, c2)) ->
        print ~at_level:2 "@[<v 0>%t;@,%t@]" (aux ~max_level:1 c1) (aux c2)
    | Do (c1, (pat, c2)) ->
        print ~at_level:2 "@[<v 0>@[<hov 2>let %t =@ %t@] in@,%t@]"
          (print_pattern pat) (aux ~max_level:1 c1) (aux c2)
    | Match (e, lst) ->
        let print_cases ppf =
          List.iter
            (fun case ->
              Format.fprintf ppf "@,| %t" (print_case rho_module case))
            lst
        in
        print "@[<v 0>match %t with%t@]"
          (print_expression rho_module ~max_level:0 e)
          print_cases
    | Apply (e1, e2) ->
        print ~at_level:1 "@[<hov 2>%t@ %t@]"
          (print_expression rho_module ~max_level:1 e1)
          (print_expression rho_module ~max_level:0 e2)
    | Delay (n, c) ->
        print ~at_level:1 "@[<hov 2>delay %d@ %t@]" n (aux ~max_level:0 c)
    | Box (rho, e, (p, c)) ->
        let rho_pp = RhoPrintParam.create () in
        print ~at_level:2 "@[<v 0>box %t %t as %t in@,%t@]"
          (print_rho rho_module rho_pp rho)
          (print_expression rho_module ~max_level:0 e)
          (print_pattern ~max_level:0 p)
          (aux c)
    | Unbox (e, (p, c)) ->
        print ~at_level:2 "@[<v 0>unbox %t as %t in@,%t@]"
          (print_expression rho_module ~max_level:0 e)
          (print_pattern p) (aux c)
    | Perform (op, e, (pat, c)) ->
        print ~at_level:1 "@[<hv 2>perform %t %t (%t.@ %t)@]" (OpName.print op)
          (print_expression rho_module ~max_level:0 e)
          (print_pattern pat) (aux ~max_level:1 c)
    | Handle (c, h) ->
        print ~at_level:1 "@[<v 0>handle@;<1 2>%t@,with %t@]" (aux c)
          (print_expression rho_module ~max_level:0 h)
  in
  aux

and print_abstraction rho_module (p, c) ppf =
  Format.fprintf ppf "@[<hv 2>%t ↦@ %t@]" (print_pattern p)
    (print_computation rho_module c)

and print_case rho_module a ppf =
  Format.fprintf ppf "%t" (print_abstraction rho_module a)

and print_op_case rho_module (op, a) ppf =
  Format.fprintf ppf "%t %t" (OpName.print op) (print_abstraction rho_module a)

(** [rho_of] reads the grade out of a context entry, which the typechecker wraps
    in what else it remembers about where the grade was accumulated. *)
let print_vars_and_tys rho_module rho_of print_var_and_ty lst ppf =
  let rec print_list = function
    | [] -> ()
    | VarMap map :: rest ->
        List.iter
          (fun entry ->
            let ty_pp = TyPrintParam.create () in
            let rho_pp = RhoPrintParam.create () in
            print_var_and_ty ty_pp rho_pp entry ppf)
          (VariableMap.bindings map);
        print_list rest
    | Rho n :: rest ->
        let rho_pp = RhoPrintParam.create () in
        print_rho rho_module rho_pp (rho_of n) ppf;
        Print.print ppf "\n";
        print_list rest
    (* A barrier has nothing to print: it carries no binding and no grade. *)
    | Barrier _ :: rest -> print_list rest
  in
  print_list (List.rev lst)

let print_vars_and_exprs rho_module print_var_and_expr
    (lst : ('var, 'map, 'rho, 'bar) Ast.context_elem_ty list) ppf =
  let print_var_map map ppf =
    let elements = VariableMap.bindings map in
    Format.fprintf ppf "@[<hv 2>{ ";
    let rec print_elements = function
      | [] -> ()
      | [ entry ] -> print_var_and_expr entry ppf
      | entry :: tl ->
          print_var_and_expr entry ppf;
          Format.fprintf ppf ",@ ";
          print_elements tl
    in
    print_elements elements;
    Format.fprintf ppf "@;<1 -2>}@]"
  in
  let print_elem ppf = function
    | VarMap map -> print_var_map map ppf
    | Rho n ->
        let rho_pp = RhoPrintParam.create () in
        print_rho rho_module rho_pp n ppf
    | Barrier _ -> ()
  in
  (* A barrier has nothing to print: it carries no binding and no grade. *)
  let elems =
    List.filter (function Barrier _ -> false | _ -> true) (List.rev lst)
  in
  match elems with
  | [] -> Format.fprintf ppf "State: []@\n"
  | _ ->
      Format.fprintf ppf "@[<v 2>State: [@,";
      let rec print_list = function
        | [] -> ()
        | [ e ] -> print_elem ppf e
        | e :: tl ->
            print_elem ppf e;
            Format.fprintf ppf ",@,";
            print_list tl
      in
      print_list elems;
      Format.fprintf ppf "@;<0 -2>]@]@\n"

(** [scheme_of] reads the type scheme out of a context entry, which the
    typechecker wraps in what else it remembers, such as where it was bound. *)
let print_variable_context rho_module rho_of scheme_of ctx =
  let print_var_and_ty ty_pp rho_pp (variable, entry) ppf =
    let { ty_params; rho_params; constrs; ty } = scheme_of entry in
    Format.fprintf ppf "@[<h>%t : %t, %t %t%t@]@." (Variable.print variable)
      (print_ty_params ty_pp ty_params)
      (print_rho_params rho_pp rho_params)
      (print_constrs rho_module ty_pp rho_pp constrs)
      (print_ty rho_module ty_pp rho_pp ty)
  in
  print_vars_and_tys rho_module rho_of print_var_and_ty ctx

let print_interpreter_state rho_module ctx ppf =
  let print_var_and_expr (variable, (rho, expr)) ppf =
    let rho_print_param = RhoPrintParam.create () in
    Format.fprintf ppf "@[<hv 2>%t ↦@ %t@ # %t@]" (Variable.print variable)
      (print_expression rho_module expr)
      (print_rho rho_module rho_print_param rho)
  in
  print_vars_and_exprs rho_module print_var_and_expr ctx ppf

let string_of_variable_context rho_module rho_of scheme_of context =
  print_variable_context rho_module rho_of scheme_of context
    Format.str_formatter;
  Format.flush_str_formatter ()

let string_of_interpreter_state rho_module context =
  print_interpreter_state rho_module context Format.str_formatter;
  Format.flush_str_formatter ()

let string_of_expression rho_module e =
  print_expression rho_module e Format.str_formatter;
  Format.flush_str_formatter ()

let string_of_computation rho_module c =
  print_computation rho_module c Format.str_formatter;
  Format.flush_str_formatter ()
