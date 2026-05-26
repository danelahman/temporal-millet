module Make (ResourceGrade : Language.ResourceGrade.S) = struct
  include Interpreter.Make (ResourceGrade)
  module Ast = Language.Ast
  module PrettyPrint = Language.PrettyPrint

  let view_run_state (run_state : run_state) =
    match run_state with
    | { computations = Ast.Return exp :: _; environment } ->
        Format.printf "%t@."
          (PrettyPrint.print_computation
             (module ResourceGrade)
             (Ast.Return exp));
        print_string
          (PrettyPrint.string_of_interpreter_state
             (module ResourceGrade)
             environment.state)
    | { computations = Ast.Perform (op, exp, cont) :: _; _ } ->
        Format.printf "%t@."
          (PrettyPrint.print_computation
             (module ResourceGrade)
             (Ast.Perform (op, exp, cont)))
    | _ -> ()
end
