include Interpreter
module PrettyPrint = Language.PrettyPrint

let view_run_state (run_state : run_state) =
  match run_state with
  | { computations = Ast.Return exp :: _; _ } ->
      Format.printf "%t@."
        (PrettyPrint.print_computation (module Tau) (Ast.Return exp))
  | { computations = Ast.Perform (op, exp, cont) :: _; _ } ->
      Format.printf "%t@."
        (PrettyPrint.print_computation
           (module Tau)
           (Ast.Perform (op, exp, cont)))
  | _ -> ()
