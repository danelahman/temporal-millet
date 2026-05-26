module SyntaxHighlight = SyntaxHighlight

module Make (ResourceGrade : Language.ResourceGrade.S) = struct
  include Interpreter.Make (ResourceGrade)
  open Vdom
  module Ast = Language.Ast
  module PrettyPrint = Language.PrettyPrint
  module RS = RedexSelectorTM.Make (ResourceGrade)

  (* Renders the interpreter state with markers around binding-position
     resource names so the syntax highlighter colors only those (and not
     resource references inside stored values). If [active] is set, the
     binding whose label equals it is additionally bracketed by the active
     marker so the web interface can highlight it like the active redex. *)
  let string_of_state ?active state =
    let label_mark ppf =
      Format.pp_print_as ppf 0
        (String.make 1 SyntaxHighlight.resource_label_marker)
    in
    let active_mark ppf =
      Format.pp_print_as ppf 0
        (String.make 1 SyntaxHighlight.active_state_marker)
    in
    let is_active variable =
      match active with
      | Some v -> Ast.Variable.compare v variable = 0
      | None -> false
    in
    let print_var_and_expr (variable, (rho, expr)) ppf =
      let rho_pp = PrettyPrint.RhoPrintParam.create () in
      let surround ppf = if is_active variable then active_mark ppf in
      Format.fprintf ppf "%t@[<hv 2>%t%t%t ↦@ %t@ # %t@]%t" surround label_mark
        (Ast.Variable.print variable)
        label_mark
        (PrettyPrint.print_expression (module ResourceGrade) expr)
        (PrettyPrint.print_rho (module ResourceGrade) rho_pp rho)
        surround
    in
    PrettyPrint.print_vars_and_exprs
      (module ResourceGrade)
      print_var_and_expr state Format.str_formatter;
    Format.flush_str_formatter ()

  (* If the redex at the head of [red] is an [Unbox] applied to a variable,
     return that variable so we can highlight the corresponding state entry. *)
  let rec active_unbox_var red c =
    match (red, c) with
    | DoCtx red, Ast.Do (c1, _) -> active_unbox_var red c1
    | HandleCtx red, Ast.Handle (c, _) -> active_unbox_var red c
    | ComputationRedex Unbox, Ast.Unbox (Ast.Var v, _) -> Some v
    | _ -> None

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

  let is_return_label = function
    | Return -> true
    | ComputationReduction _ -> false

  let is_done = function { computations = []; _ } -> true | _ -> false

  let view_run_state (run_state : run_state) step_label =
    match run_state with
    | { environment; computations = comp :: _ } ->
        let reduction =
          match step_label with
          | Some (ComputationReduction red) -> Some red
          | Some Return -> None
          | None -> None
        in

        let active =
          match reduction with
          | Some red -> active_unbox_var red comp
          | None -> None
        in
        let state_string = string_of_state ?active environment.state in
        let state_nodes =
          match
            String.split_on_char SyntaxHighlight.active_state_marker
              state_string
          with
          | [ code ] -> SyntaxHighlight.highlight_text code
          | [ pre; active; post ] ->
              SyntaxHighlight.highlight_text pre
              @ [
                  elt "span"
                    ~a:[ class_ "active-redex" ]
                    (SyntaxHighlight.highlight_text active);
                ]
              @ SyntaxHighlight.highlight_text post
          | _ -> SyntaxHighlight.highlight_text state_string
        in
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
