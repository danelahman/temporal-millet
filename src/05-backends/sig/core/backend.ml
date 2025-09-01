module Ast = Language.Ast
module Tau = Language.Tau.TauImpl
module Context = Language.Context
module Primitives = Language.Primitives

module ContextHolderModule =
  Context.Make (Ast.Variable) (Map.Make (Ast.Variable)) (Tau)

module type S = sig
  type load_state

  type evaluation_environment = {
    state : (Tau.t Ast.tau * Tau.t Ast.expression) ContextHolderModule.t;
    variables : Tau.t Ast.expression ContextHolderModule.t;
    builtin_functions :
      (Tau.t Ast.expression -> Tau.t Ast.computation) ContextHolderModule.t;
    resource_counter : int;
  }

  val initial_load_state : load_state

  val load_primitive :
    load_state -> Ast.variable -> Primitives.primitive -> load_state

  val load_ty_def :
    load_state ->
    (Ast.ty_param list * Ast.ty_name * Tau.t Ast.ty_def) list ->
    load_state

  val load_top_let :
    load_state -> Ast.variable -> Tau.t Ast.expression -> load_state

  val load_top_do : load_state -> Tau.t Ast.computation -> load_state

  type run_state
  type step_label

  type step = {
    environment : evaluation_environment;
    label : step_label;
    next_state : unit -> run_state;
  }

  val run : load_state -> run_state
  val steps : run_state -> step list
end
