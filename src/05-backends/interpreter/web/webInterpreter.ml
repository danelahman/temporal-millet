module Make (ResourceGrade : Language.ResourceGrade.S) = struct
  include Interpreter.Make (ResourceGrade)
  open Vdom
  module Ast = Language.Ast
  module PrettyPrint = Language.PrettyPrint
  module RS = RedexSelectorTM.Make (ResourceGrade)

  (* Renders the interpreter state with markers around binding-position
     resource names so the syntax highlighter colors only those (and not
     resource references inside stored values). *)
  let string_of_state state =
    let mark ppf =
      Format.pp_print_as ppf 0
        (String.make 1 SyntaxHighlight.resource_label_marker)
    in
    let print_var_and_expr (variable, (rho, expr)) ppf =
      let rho_pp = PrettyPrint.RhoPrintParam.create () in
      Format.fprintf ppf "@[<hv 2>%t%t%t ↦@ %t@ # %t@]" mark
        (Ast.Variable.print variable)
        mark
        (PrettyPrint.print_expression (module ResourceGrade) expr)
        (PrettyPrint.print_rho (module ResourceGrade) rho_pp rho)
    in
    PrettyPrint.print_vars_and_exprs
      (module ResourceGrade)
      print_var_and_expr state Format.str_formatter;
    Format.flush_str_formatter ()

  let view_computation_redex = function
    | Match -> "match"
    | ApplyFun -> "applyFun"
    | DoReturn -> "doReturn"
    | DoOp -> "doOp"
    | Delay -> "delay"
    | Box -> "box"
    | Unbox -> "unbox"
    | HandleReturn -> "handleReturn"
    | HandleOp -> "handleOp"

  let rec view_computation_reduction = function
    | DoCtx red -> view_computation_reduction red
    | HandleCtx red -> view_computation_reduction red
    | ComputationRedex redex -> view_computation_redex redex

  let view_step_label = function
    | ComputationReduction reduction ->
        text (view_computation_reduction reduction)
    | Return -> text "return"

  let view_run_state (run_state : run_state) step_label =
    match run_state with
    | { environment; computations = comp :: _ } ->
        let reduction =
          match step_label with
          | Some (ComputationReduction red) -> Some red
          | Some Return -> None
          | None -> None
        in

        let state_string = string_of_state environment.state in
        let state_nodes = SyntaxHighlight.highlight_text state_string in
        let computation_tree =
          RS.view_computation_with_redexes reduction comp
        in
        div
          ~a:[ class_ "box" ]
          [
            elt "pre" ~a:[ class_ "syn-ml" ] state_nodes;
            elt "hr" [];
            elt "pre" ~a:[ class_ "syn-ml" ] computation_tree;
          ]
    | { computations = []; _ } ->
        div ~a:[ class_ "box" ] [ elt "pre" [ text "done" ] ]
end
