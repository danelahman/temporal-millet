module Ast = Language.Ast
module Primitives = Language.Primitives

module type S = sig
  module Tau : Language.Tau.S

  type load_state

  type evaluation_environment = {
    state :
      ( Ast.Variable.t,
        (Tau.t Ast.tau * Tau.t Ast.expression) Ast.VariableMap.t,
        Tau.t Ast.tau )
      Ast.context_elem_ty
      list;
    variables :
      ( Ast.Variable.t,
        Tau.t Ast.expression Ast.VariableMap.t,
        Tau.t Ast.tau )
      Ast.context_elem_ty
      list;
    builtin_functions :
      ( Ast.Variable.t,
        (Tau.t Ast.expression -> Tau.t Ast.computation) Ast.VariableMap.t,
        Tau.t Ast.tau )
      Ast.context_elem_ty
      list;
    resource_counter : int;
    op_signatures : Tau.t Ast.tau Ast.OpNameMap.t;
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
  val load_op_sig : load_state -> Ast.OpName.t -> Tau.t Ast.tau -> load_state

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
