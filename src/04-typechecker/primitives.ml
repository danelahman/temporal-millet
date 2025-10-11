module Ast = Language.Ast
module Tau = Language.Tau.TauImpl
module Context = Language.Context
module Const = Language.Const
module Primitives = Language.Primitives

let poly_type ty =
  let a = Ast.TyParamModule.fresh "poly" in
  ([ a ], [], ty (Ast.TyParam a))

let unary_integer_op_ty tau =
  ( [],
    [],
    Ast.TyArrow
      ( Ast.TyConst Const.IntegerTy,
        Ast.CompTy (Ast.TyConst Const.IntegerTy, tau) ) )

let binary_integer_op_ty tau =
  ( [],
    [],
    Ast.TyArrow
      ( Ast.TyTuple [ Ast.TyConst Const.IntegerTy; Ast.TyConst Const.IntegerTy ],
        Ast.CompTy (Ast.TyConst Const.IntegerTy, tau) ) )

let unary_float_op_ty tau =
  ( [],
    [],
    Ast.TyArrow
      (Ast.TyConst Const.FloatTy, Ast.CompTy (Ast.TyConst Const.FloatTy, tau))
  )

let binary_float_op_ty tau =
  ( [],
    [],
    Ast.TyArrow
      ( Ast.TyTuple [ Ast.TyConst Const.FloatTy; Ast.TyConst Const.FloatTy ],
        Ast.CompTy (Ast.TyConst Const.FloatTy, tau) ) )

let binary_string_op_ty tau =
  ( [],
    [],
    Ast.TyArrow
      ( Ast.TyTuple [ Ast.TyConst Const.StringTy; Ast.TyConst Const.StringTy ],
        Ast.CompTy (Ast.TyConst Const.StringTy, tau) ) )

let comparison_ty tau =
  poly_type (fun a ->
      Ast.TyArrow
        (Ast.TyTuple [ a; a ], Ast.CompTy (Ast.TyConst Const.BooleanTy, tau)))

let primitive_type_scheme = function
  | Primitives.CompareEq -> comparison_ty (Ast.TauConst Tau.zero)
  | Primitives.CompareLt -> comparison_ty (Ast.TauConst Tau.zero)
  | Primitives.CompareGt -> comparison_ty (Ast.TauConst Tau.zero)
  | Primitives.CompareLe -> comparison_ty (Ast.TauConst Tau.zero)
  | Primitives.CompareGe -> comparison_ty (Ast.TauConst Tau.zero)
  | Primitives.CompareNe -> comparison_ty (Ast.TauConst Tau.zero)
  | Primitives.IntegerAdd -> binary_integer_op_ty (Ast.TauConst Tau.zero)
  | Primitives.IntegerMul -> binary_integer_op_ty (Ast.TauConst Tau.zero)
  | Primitives.IntegerSub -> binary_integer_op_ty (Ast.TauConst Tau.zero)
  | Primitives.IntegerDiv -> binary_integer_op_ty (Ast.TauConst Tau.zero)
  | Primitives.IntegerMod -> binary_integer_op_ty (Ast.TauConst Tau.zero)
  | Primitives.IntegerNeg -> unary_integer_op_ty (Ast.TauConst Tau.zero)
  | Primitives.FloatAdd -> binary_float_op_ty (Ast.TauConst Tau.zero)
  | Primitives.FloatMul -> binary_float_op_ty (Ast.TauConst Tau.zero)
  | Primitives.FloatSub -> binary_float_op_ty (Ast.TauConst Tau.zero)
  | Primitives.FloatDiv -> binary_float_op_ty (Ast.TauConst Tau.zero)
  | Primitives.FloatPow -> binary_float_op_ty (Ast.TauConst Tau.zero)
  | Primitives.FloatNeg -> unary_float_op_ty (Ast.TauConst Tau.zero)
  | Primitives.StringCat -> binary_string_op_ty (Ast.TauConst Tau.zero)
  | Primitives.ToString ->
      poly_type (fun a ->
          Ast.TyArrow
            (a, Ast.CompTy (Ast.TyConst Const.StringTy, Ast.TauConst Tau.zero)))
