module Make (ResourceGrade : Language.ResourceGrade.Grade) = struct
  include Interpreter.Make (ResourceGrade)
  module Ast = Language.Ast
  module PrettyPrint = Language.PrettyPrint

  let view_run_state (run_state : run_state) ~run_num =
    match run_state with
    | { computations = Ast.Return exp :: _; environment } ->
        Format.printf "=== Run %d ===@." run_num;
        Format.printf "%t@."
          (PrettyPrint.print_computation
             (module ResourceGrade)
             (Ast.Return exp));
        print_string
          (PrettyPrint.string_of_interpreter_state
             (module ResourceGrade)
             environment.state);
        print_newline ();
        true
    (* An operation with a default implementation is not stuck here: the
       default is about to fire, so there is nothing to report yet. *)
    | { computations = Ast.Perform (op, exp, cont) :: _; environment }
      when not (Ast.OpNameMap.mem op environment.op_defaults) ->
        Format.printf "=== Run %d (unhandled operation) ===@." run_num;
        Format.printf "%t@."
          (PrettyPrint.print_computation
             (module ResourceGrade)
             (Ast.Perform (op, exp, cont)));
        print_newline ();
        true
    | _ -> false
end
