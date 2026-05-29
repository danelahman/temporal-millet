module Make (GS : Language.GradeSystem.S) = struct
  include Interpreter.Make (GS)

  let view_run_state (run_state : run_state) ~run_num =
    match run_state with
    | { computations = Ast.Return exp :: _; environment } ->
        Format.printf "=== Run %d ===@." run_num;
        Format.printf "%t@." (PP.print_computation (Ast.Return exp));
        print_string (PP.string_of_interpreter_state environment.state);
        print_newline ();
        true
    | { computations = Ast.Perform (op, exp, cont) :: _; _ } ->
        Format.printf "=== Run %d (unhandled operation) ===@." run_num;
        Format.printf "%t@."
          (PP.print_computation (Ast.Perform (op, exp, cont)));
        print_newline ();
        true
    | _ -> false
end
