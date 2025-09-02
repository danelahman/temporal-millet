module Ast = Language.Ast

type 'a t =
  | TypeConstraint of ('a Ast.ty * 'a Ast.ty)
  | TauConstraint of ('a Ast.tau * 'a Ast.tau)
  | TauGeq of ('a Ast.tau * 'a Ast.tau)
  | FreshTauConstraint of 'a Ast.tau
