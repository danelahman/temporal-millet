(** Desugaring of [Parser.SugaredAst] commands into [Language.Ast] commands.

    [Make] is parameterised by a resource grade so that surface syntax for grade
    literals can be lowered to the appropriate AST values. Internal helpers
    ([desugar_ty], [desugar_pattern], etc.) are intentionally hidden — only the
    command-level entry points are part of the public API. *)

module Make (ResourceGrade : Language.ResourceGrade.S) : sig
  type state

  val initial_state : state

  val load_primitive :
    state -> Language.Ast.variable -> Language.Primitives.primitive -> state

  val desugar_command :
    state ->
    ResourceGrade.t Parser.SugaredAst.command ->
    state * ResourceGrade.t Language.Ast.command
end
