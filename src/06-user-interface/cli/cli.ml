module Error = Utils.Error
module PrettyPrint = Language.PrettyPrint

type config = {
  filenames : string list;
  use_stdlib : bool;
  debug : bool;
  tau_type : string;
}

let accepted_tau_names = List.map fst Language.Tau.tau_modules
let default_tau_name = List.hd accepted_tau_names

let parse_args_to_config () =
  let filenames = ref []
  and use_stdlib = ref true
  and debug = ref false
  and tau_type = ref default_tau_name in
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
        ( "--resources",
          Arg.Set_string tau_type,
          Printf.sprintf
            " Type of resource grades to use (default: %s). Accepted: %s"
            default_tau_name
            (String.concat ", "
               (List.map (fun s -> "'" ^ s ^ "'") accepted_tau_names)) );
      ]
  in
  Arg.parse options anonymous usage;
  {
    filenames = List.rev !filenames;
    use_stdlib = !use_stdlib;
    debug = !debug;
    tau_type = !tau_type;
  }

let run_with (type t) (module Tau : Language.Tau.S with type t = t) config =
  let module Backend = CliInterpreter.Make (Tau) in
  let module Loader = Loader.Loader (Backend) in
  let rec run (state : Backend.run_state) debug =
    Backend.view_run_state state;
    match Backend.steps state with
    | [] -> ()
    | steps ->
        let i = Random.int (List.length steps) in
        let step = List.nth steps i in
        let state' = step.next_state () in
        if debug && Backend.steps state' = [] then
          print_string
            (PrettyPrint.string_of_interpreter_state
               (module Tau)
               step.environment.state);
        run state' debug
  in
  try
    Random.self_init ();
    let state =
      if config.use_stdlib then
        Loader.load_source Loader.initial_state Loader.stdlib_source
      else Loader.initial_state
    in
    let state' = List.fold_left Loader.load_file state config.filenames in
    let run_state = Backend.run state'.backend in
    if config.debug then
      print_string
        (PrettyPrint.string_of_variable_context
           (module Tau)
           state'.typechecker.variables);
    run run_state config.debug
  with Error.Error error ->
    Error.print error;
    exit 1

let main () =
  let config = parse_args_to_config () in
  match List.assoc_opt config.tau_type Language.Tau.tau_modules with
  | Some (module Tau : Language.Tau.S) -> run_with (module Tau) config
  | None ->
      Printf.eprintf "Unknown type of resource grades '%s'. Accepted: %s\n"
        config.tau_type
        (String.concat ", "
           (List.map (fun s -> "'" ^ s ^ "'") accepted_tau_names));
      exit 1

let _ = main ()
