include Interpreter
open Vdom
module PrettyPrint = Language.PrettyPrint

let view_computation_redex = function
  | Interpreter.Match -> "match"
  | Interpreter.ApplyFun -> "applyFun"
  | Interpreter.DoReturn -> "doReturn"
  | Interpreter.DoOp -> "doOp"
  | Interpreter.Delay -> "delay"
  | Interpreter.Box -> "box"
  | Interpreter.Unbox -> "unbox"
  | Interpreter.HandleReturn -> "handleReturn"
  | Interpreter.HandleOp -> "handleOp"

let rec view_computation_reduction = function
  | Interpreter.DoCtx red -> view_computation_reduction red
  | Interpreter.HandleCtx red -> view_computation_reduction red
  | Interpreter.ComputationRedex redex -> view_computation_redex redex

let view_step_label = function
  | Interpreter.ComputationReduction reduction ->
      text (view_computation_reduction reduction)
  | Interpreter.Return -> text "return"

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
        RedexSelectorTM.view_computation_with_redexes reduction comp
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
