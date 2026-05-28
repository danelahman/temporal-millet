module Ast = Language.Ast
module Context = Language.Context
module Const = Language.Const
module Primitives = Language.Primitives

module Make (ResourceGrade : Language.ResourceGrade.Grade) = struct
  let poly_type ty =
    let a = Ast.TyParamModule.fresh "poly" in
    ([ a ], [], ty (Ast.TyParam a))

  let unary_integer_op_ty rho =
    ( [],
      [],
      Ast.TyArrow
        ( Ast.TyConst Const.IntegerTy,
          Ast.CompTy (Ast.TyConst Const.IntegerTy, rho) ) )

  let binary_integer_op_ty rho =
    ( [],
      [],
      Ast.TyArrow
        ( Ast.TyTuple
            [ Ast.TyConst Const.IntegerTy; Ast.TyConst Const.IntegerTy ],
          Ast.CompTy (Ast.TyConst Const.IntegerTy, rho) ) )

  let unary_float_op_ty rho =
    ( [],
      [],
      Ast.TyArrow
        (Ast.TyConst Const.FloatTy, Ast.CompTy (Ast.TyConst Const.FloatTy, rho))
    )

  let binary_float_op_ty rho =
    ( [],
      [],
      Ast.TyArrow
        ( Ast.TyTuple [ Ast.TyConst Const.FloatTy; Ast.TyConst Const.FloatTy ],
          Ast.CompTy (Ast.TyConst Const.FloatTy, rho) ) )

  let comparison_ty rho =
    poly_type (fun a ->
        Ast.TyArrow
          (Ast.TyTuple [ a; a ], Ast.CompTy (Ast.TyConst Const.BooleanTy, rho)))

  let primitive_type_scheme = function
    | Primitives.CompareEq -> comparison_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.CompareLt -> comparison_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.CompareGt -> comparison_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.CompareLe -> comparison_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.CompareGe -> comparison_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.CompareNe -> comparison_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.IntegerAdd ->
        binary_integer_op_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.IntegerMul ->
        binary_integer_op_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.IntegerSub ->
        binary_integer_op_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.IntegerDiv ->
        binary_integer_op_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.IntegerMod ->
        binary_integer_op_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.IntegerNeg ->
        unary_integer_op_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.FloatAdd ->
        binary_float_op_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.FloatMul ->
        binary_float_op_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.FloatSub ->
        binary_float_op_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.FloatDiv ->
        binary_float_op_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.FloatPow ->
        binary_float_op_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.FloatNeg -> unary_float_op_ty (Ast.RhoConst ResourceGrade.zero)
    | Primitives.ToString ->
        poly_type (fun a ->
            Ast.TyArrow
              ( a,
                Ast.CompTy
                  (Ast.TyConst Const.StringTy, Ast.RhoConst ResourceGrade.zero)
              ))
end
