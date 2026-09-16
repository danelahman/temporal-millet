module Error = Utils.Error
module Ast = Language.Ast
module Const = Language.Const
module Primitives = Language.Primitives
module PrettyPrint = Language.PrettyPrint

module Make (ResourceGrade : Language.ResourceGrade.Grade) = struct
  (* A primitive's result is not written anywhere in the source, so it is
     reported at the span of the argument it was computed from. *)
  let return_const at c =
    Ast.located at (Ast.Return (Ast.located at (Ast.Const c)))

  let binary_function f (expr : _ Ast.expression) =
    match expr.it with
    | Ast.Tuple [ expr1; expr2 ] -> f expr1 expr2
    | _ ->
        Error.runtime "Pair expected but got %t"
          (PrettyPrint.print_expression (module ResourceGrade) expr)

  let get_int (expr : _ Ast.expression) =
    match expr.it with
    | Ast.Const (Const.Integer n) -> n
    | _ ->
        Error.runtime "Integer expected but got %t"
          (PrettyPrint.print_expression (module ResourceGrade) expr)

  let get_float (expr : _ Ast.expression) =
    match expr.it with
    | Ast.Const (Const.Float n) -> n
    | _ ->
        Error.runtime "Float expected but got %t"
          (PrettyPrint.print_expression (module ResourceGrade) expr)

  let int_to f expr =
    let n = get_int expr in
    f n

  let int_int_to f expr =
    binary_function
      (fun expr1 expr2 ->
        let n1 = get_int expr1 in
        let n2 = get_int expr2 in
        f n1 n2)
      expr

  let float_to f expr =
    let n = get_float expr in
    f n

  let float_float_to f expr =
    binary_function
      (fun expr1 expr2 ->
        let n1 = get_float expr1 in
        let n2 = get_float expr2 in
        f n1 n2)
      expr

  let int_to_int f expr =
    return_const expr.Ast.at (Const.Integer (int_to f expr))

  let int_int_to_int f expr =
    return_const expr.Ast.at (Const.Integer (int_int_to f expr))

  let float_to_float f expr =
    return_const expr.Ast.at (Const.Float (float_to f expr))

  let float_float_to_float f expr =
    return_const expr.Ast.at (Const.Float (float_float_to f expr))

  (* Which shapes the comparison primitives below accept: everything but a
     function or a handler, which are not values that can be compared. *)
  let rec comparable_expression (expr : _ Ast.expression) =
    match expr.it with
    | Ast.Var _ -> true
    | Const _ -> true
    | Annotated (e, _) -> comparable_expression e
    | Tuple es -> List.for_all comparable_expression es
    | Variant (_, e) -> Option.fold ~none:true ~some:comparable_expression e
    | Lambda _ -> false
    | PureLambda _ -> false
    | RecLambda _ -> false
    | Handler _ -> false

  let comparison f (expr : _ Ast.expression) =
    binary_function
      (fun e1 e2 ->
        if not (comparable_expression e1) then
          Error.runtime "Incomparable expression %t"
            (PrettyPrint.print_expression
               (module ResourceGrade)
               ~max_level:0 e1)
        else if not (comparable_expression e2) then
          Error.runtime "Incomparable expression %t"
            (PrettyPrint.print_expression
               (module ResourceGrade)
               ~max_level:0 e2)
        else
          return_const expr.at
            (Const.Boolean (f (Ast.compare_expression e1 e2) 0)))
      expr

  let primitive_function = function
    | Primitives.CompareEq -> comparison ( = )
    | Primitives.CompareLt -> comparison ( < )
    | Primitives.CompareGt -> comparison ( > )
    | Primitives.CompareLe -> comparison ( <= )
    | Primitives.CompareGe -> comparison ( >= )
    | Primitives.CompareNe -> comparison ( <> )
    | Primitives.IntegerAdd -> int_int_to_int ( + )
    | Primitives.IntegerMul -> int_int_to_int ( * )
    | Primitives.IntegerSub -> int_int_to_int ( - )
    | Primitives.IntegerDiv -> int_int_to_int ( / )
    | Primitives.IntegerMod -> int_int_to_int ( mod )
    | Primitives.IntegerNeg -> int_to_int ( ~- )
    | Primitives.FloatAdd -> float_float_to_float ( +. )
    | Primitives.FloatMul -> float_float_to_float ( *. )
    | Primitives.FloatSub -> float_float_to_float ( -. )
    | Primitives.FloatDiv -> float_float_to_float ( /. )
    | Primitives.FloatPow -> float_float_to_float ( ** )
    | Primitives.FloatNeg -> float_to_float ( ~-. )
    | Primitives.ToString ->
        fun expr ->
          return_const expr.Ast.at
            (Const.String
               (PrettyPrint.string_of_expression (module ResourceGrade) expr))
end
