module Make (Resource : Language.Resource.S) = struct
  include Interpreter.Make (Resource)
  module Ast = Language.Ast
  module PrettyPrint = Language.PrettyPrint

  let view_run_state (run_state : run_state) =
    match run_state with
    | { computations = Ast.Return exp :: _; _ } ->
        Format.printf "%t@."
          (PrettyPrint.print_computation (module Resource) (Ast.Return exp))
    | { computations = Ast.Perform (op, exp, cont) :: _; _ } ->
        Format.printf "%t@."
          (PrettyPrint.print_computation
             (module Resource)
             (Ast.Perform (op, exp, cont)))
    | _ -> ()
end
