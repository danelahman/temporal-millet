module Error = Utils.Error
module Diagnostic = Utils.Diagnostic
module Ast = Language.Ast
module PrettyPrint = Language.PrettyPrint

(* [Loader] is shadowed inside [run_with] by the backend's instance of the
   functor, so the name is bound here while the library module is in scope. *)
let stdlib_filename = Loader.stdlib_filename

(* What [Diagnostic.print] quotes the source from. The standard library is
   loaded from a string and has no file to read; a location with no file name
   at all comes from a source only the caller knows, so it is left unquoted. *)
let source filename =
  if filename = stdlib_filename then Some Loader.stdlib_source
  else if filename = "" then None
  else
    try Some (In_channel.with_open_text filename In_channel.input_all)
    with Sys_error _ -> None

let user_defined_variables ~stdlib_vars ~final_vars =
  let in_stdlib var =
    List.exists
      (function
        | Ast.VarMap m -> Ast.VariableMap.mem var m
        | Ast.Rho _ | Ast.Barrier _ -> false)
      stdlib_vars
  in
  List.map
    (function
      | Ast.VarMap m ->
          Ast.VarMap (Ast.VariableMap.filter (fun k _ -> not (in_stdlib k)) m)
      | (Ast.Rho _ | Ast.Barrier _) as r -> r)
    final_vars

type config = {
  filenames : string list;
  use_stdlib : bool;
  debug : bool;
  typecheck_only : bool;
  resource_type : string;
}

let accepted_resource_names =
  List.map fst Language.ResourceGrade.resource_grade_modules

let default_resource_name = List.hd accepted_resource_names

let parse_args_to_config () =
  let filenames = ref []
  and use_stdlib = ref true
  and debug = ref false
  and typecheck_only = ref false
  and resource_type = ref default_resource_name in
  let usage = "Run Temporal Millet as '" ^ Sys.argv.(0) ^ " [filename.mlt] ...'"
  and anonymous filename = filenames := filename :: !filenames
  (* The options, in alphabetical order. [--help] is listed rather than left
     to [Arg] to add, which would put it last; it refers back to the list to
     print it, hence the reference. *)
  and options = ref [] in
  options :=
    Arg.align
      [
        ( "--debug",
          Arg.Set debug,
          " Show final internal state and top level typing results after \
           execution" );
        ( "--help",
          Arg.Unit
            (fun () -> raise (Arg.Help (Arg.usage_string !options usage))),
          " Display this list of options" );
        ( "--no-stdlib",
          Arg.Clear use_stdlib,
          " Do not load the standard library" );
        ( "--resources",
          Arg.Set_string resource_type,
          Printf.sprintf
            " Type of resource grades to use (default: %s). Accepted: %s"
            default_resource_name
            (String.concat ", "
               (List.map (fun s -> "'" ^ s ^ "'") accepted_resource_names)) );
        ( "--typecheck-only",
          Arg.Set typecheck_only,
          " Typecheck the files without running them" );
        (* [Arg] would otherwise add a single-dash "-help" alongside "--help";
           listing it here with an empty doc string keeps it out of the help
           text and makes it fail like any other unknown option. *)
        ( "-help",
          Arg.Unit (fun () -> raise (Arg.Bad "unknown option '-help'")),
          "" );
      ];
  Arg.parse !options anonymous usage;
  {
    filenames = List.rev !filenames;
    use_stdlib = !use_stdlib;
    debug = !debug;
    typecheck_only = !typecheck_only;
    resource_type = !resource_type;
  }

let run_with (type t)
    (module ResourceGrade : Language.ResourceGrade.Grade with type t = t) config
    =
  let module Backend = CliInterpreter.Make (ResourceGrade) in
  let module Loader = Loader.Loader (Backend) in
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
        Loader.load_source ~filename:stdlib_filename Loader.initial_state
          Loader.stdlib_source
      else Loader.initial_state
    in
    (* Every file is loaded even when an earlier one had errors, so that all
       of them are reported at once; a fatal failure still stops everything. *)
    let state', diagnostics =
      List.fold_left
        (fun (state, diagnostics) filename ->
          let state', diagnostics' = Loader.load_file_all state filename in
          (state', diagnostics @ diagnostics'))
        (stdlib_state, []) config.filenames
    in
    (* A blank line between diagnostics, so that a reader can tell where one
       ends. A rejected program is not run, whatever [--typecheck-only] says. *)
    if diagnostics <> [] then begin
      List.iteri
        (fun i d ->
          if i > 0 then Format.pp_print_newline Format.err_formatter ();
          Diagnostic.print ~source d Format.err_formatter)
        diagnostics;
      exit 1
    end;
    let run_state = Backend.run state'.backend in
    if config.debug then begin
      if config.use_stdlib then begin
        print_endline "=== Standard library ===";
        print_string
          (PrettyPrint.string_of_variable_context
             (module ResourceGrade)
             Loader.TC.Elapsed.rho Loader.TC.scheme_of
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
           Loader.TC.Elapsed.rho Loader.TC.scheme_of user_vars);
      print_newline ()
    end;
    (* loading the files has typechecked every command, the [run]s included *)
    if not config.typecheck_only then run run_state 1
  with Error.Error d ->
    Diagnostic.print ~source d Format.err_formatter;
    exit 1

let main () =
  let config = parse_args_to_config () in
  match
    List.assoc_opt config.resource_type
      Language.ResourceGrade.resource_grade_modules
  with
  | Some (module ResourceGrade : Language.ResourceGrade.Grade) ->
      run_with (module ResourceGrade) config
  | None ->
      Printf.eprintf "Unknown type of resource grades '%s'. Accepted: %s\n"
        config.resource_type
        (String.concat ", "
           (List.map (fun s -> "'" ^ s ^ "'") accepted_resource_names));
      exit 1

let _ = main ()
