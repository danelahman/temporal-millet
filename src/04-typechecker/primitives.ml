module Ast = Language.Ast
module Context = Language.Context
module Const = Language.Const
module Primitives = Language.Primitives

module Make (Resource : Language.Resource.S) = struct
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
    | Primitives.CompareEq -> comparison_ty (Ast.RhoConst Resource.zero)
    | Primitives.CompareLt -> comparison_ty (Ast.RhoConst Resource.zero)
    | Primitives.CompareGt -> comparison_ty (Ast.RhoConst Resource.zero)
    | Primitives.CompareLe -> comparison_ty (Ast.RhoConst Resource.zero)
    | Primitives.CompareGe -> comparison_ty (Ast.RhoConst Resource.zero)
    | Primitives.CompareNe -> comparison_ty (Ast.RhoConst Resource.zero)
    | Primitives.IntegerAdd -> binary_integer_op_ty (Ast.RhoConst Resource.zero)
    | Primitives.IntegerMul -> binary_integer_op_ty (Ast.RhoConst Resource.zero)
    | Primitives.IntegerSub -> binary_integer_op_ty (Ast.RhoConst Resource.zero)
    | Primitives.IntegerDiv -> binary_integer_op_ty (Ast.RhoConst Resource.zero)
    | Primitives.IntegerMod -> binary_integer_op_ty (Ast.RhoConst Resource.zero)
    | Primitives.IntegerNeg -> unary_integer_op_ty (Ast.RhoConst Resource.zero)
    | Primitives.FloatAdd -> binary_float_op_ty (Ast.RhoConst Resource.zero)
    | Primitives.FloatMul -> binary_float_op_ty (Ast.RhoConst Resource.zero)
    | Primitives.FloatSub -> binary_float_op_ty (Ast.RhoConst Resource.zero)
    | Primitives.FloatDiv -> binary_float_op_ty (Ast.RhoConst Resource.zero)
    | Primitives.FloatPow -> binary_float_op_ty (Ast.RhoConst Resource.zero)
    | Primitives.FloatNeg -> unary_float_op_ty (Ast.RhoConst Resource.zero)
    | Primitives.ToString ->
        poly_type (fun a ->
            Ast.TyArrow
              ( a,
                Ast.CompTy
                  (Ast.TyConst Const.StringTy, Ast.RhoConst Resource.zero) ))
end
