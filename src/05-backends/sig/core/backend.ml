module Primitives = Language.Primitives

module type S = sig
  module GradeSystem : Language.GradeSystem.S
  module Ast : module type of Language.Ast.Make (GradeSystem)

  type load_state

  type evaluation_environment = {
    state : (Ast.resource_grade * Ast.expression) Ast.context_elem_ty list;
    variables : Ast.expression Ast.context_elem_ty list;
    builtin_functions :
      (Ast.expression -> Ast.computation) Ast.context_elem_ty list;
    resource_counter : int;
    op_signatures : Ast.resource_grade Ast.OpNameMap.t;
  }

  val initial_load_state : load_state

  val load_primitive :
    load_state -> Ast.variable -> Primitives.primitive -> load_state

  val load_ty_def :
    load_state ->
    (Ast.ty_param list * Ast.ty_name * Ast.ty_def) list ->
    load_state

  val load_top_let : load_state -> Ast.variable -> Ast.expression -> load_state
  val load_top_do : load_state -> Ast.computation -> load_state

  val load_op_sig :
    load_state -> Ast.OpName.t -> Ast.resource_grade -> load_state

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
