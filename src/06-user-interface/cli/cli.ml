module Error = Utils.Error
module Ast = Language.Ast
module PrettyPrint = Language.PrettyPrint

let user_defined_variables ~stdlib_vars ~final_vars =
  let in_stdlib var =
    List.exists
      (function
        | Ast.VarMap m -> Ast.VariableMap.mem var m | Ast.Rho _ -> false)
      stdlib_vars
  in
  List.map
    (function
      | Ast.VarMap m ->
          Ast.VarMap (Ast.VariableMap.filter (fun k _ -> not (in_stdlib k)) m)
      | Ast.Rho _ as r -> r)
    final_vars

type config = {
  filenames : string list;
  use_stdlib : bool;
  debug : bool;
  grade_type : string;
}

let accepted_grade_names = List.map fst Language.GradeSystem.grade_systems
let default_grade_name = List.hd accepted_grade_names

let parse_args_to_config () =
  let filenames = ref []
  and use_stdlib = ref true
  and debug = ref false
  and grade_type = ref default_grade_name in
  let usage = "Run Temporal Millet as '" ^ Sys.argv.(0) ^ " [filename.mlt] ...'"
  and anonymous filename = filenames := filename :: !filenames
  and options =
    Arg.align
      [
        ( "--no-stdlib",
          Arg.Clear use_stdlib,
          " Do not load the standard library" );
        ( "--debug",
          Arg.Set debug,
          " Show final internal state and top level typing results after \
           execution" );
        ( "--grades",
          Arg.Set_string grade_type,
          Printf.sprintf " Type of grades to use (default: %s). Accepted: %s"
            default_grade_name
            (String.concat ", "
               (List.map (fun s -> "'" ^ s ^ "'") accepted_grade_names)) );
      ]
  in
  Arg.parse options anonymous usage;
  {
    filenames = List.rev !filenames;
    use_stdlib = !use_stdlib;
    debug = !debug;
    grade_type = !grade_type;
  }

let run_with (module GS : Language.GradeSystem.S) config =
  let module Backend = CliInterpreter.Make (GS) in
  let module Loader = Loader.Loader (Backend) in
  let module ResourceGrade = GS.ResourceGrade in
  let rec run (state : Backend.run_state) run_num =
    let printed = Backend.view_run_state state ~run_num in
    let next_run_num = if printed then run_num + 1 else run_num in
    match Backend.steps state with
    | [] -> ()
    | steps ->
        let i = Random.int (List.length steps) in
        let step = List.nth steps i in
        let state' = step.next_state () in
        run state' next_run_num
  in
  try
    Random.self_init ();
    let stdlib_state =
      if config.use_stdlib then
        Loader.load_source Loader.initial_state Loader.stdlib_source
      else Loader.initial_state
    in
    let state' =
      List.fold_left Loader.load_file stdlib_state config.filenames
    in
    let run_state = Backend.run state'.backend in
    if config.debug then begin
      if config.use_stdlib then begin
        print_endline "=== Standard library ===";
        print_string
          (PrettyPrint.string_of_variable_context
             (module ResourceGrade)
             stdlib_state.typechecker.variables);
        print_newline ()
      end;
      print_endline "=== Top-level definitions ===";
      let user_vars =
        user_defined_variables ~stdlib_vars:stdlib_state.typechecker.variables
          ~final_vars:state'.typechecker.variables
      in
      print_string
        (PrettyPrint.string_of_variable_context
           (module ResourceGrade)
           user_vars);
      print_newline ()
    end;
    run run_state 1
  with Error.Error error ->
    Error.print error;
    exit 1

let main () =
  let config = parse_args_to_config () in
  match List.assoc_opt config.grade_type Language.GradeSystem.grade_systems with
  | Some (module GS : Language.GradeSystem.S) -> run_with (module GS) config
  | None ->
      Printf.eprintf "Unknown type of grades '%s'. Accepted: %s\n"
        config.grade_type
        (String.concat ", "
           (List.map (fun s -> "'" ^ s ^ "'") accepted_grade_names));
      exit 1

let _ = main ()
