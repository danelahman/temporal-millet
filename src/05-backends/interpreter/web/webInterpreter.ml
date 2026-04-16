module Make (Tau : Language.Tau.S) = struct
  include Interpreter.Make (Tau)
  open Vdom
  module PrettyPrint = Language.PrettyPrint
  module RS = RedexSelectorTM.Make (Tau)

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

        let state_string =
          PrettyPrint.string_of_interpreter_state (module Tau) environment.state
        in
        let computation_tree =
          RS.view_computation_with_redexes reduction comp
        in
        div
          ~a:[ class_ "box" ]
          [
            elt "pre" [ text state_string ];
            elt "hr" [];
            elt "pre" computation_tree;
          ]
    | { computations = []; _ } ->
        div ~a:[ class_ "box" ] [ elt "pre" [ text "done" ] ]
end
