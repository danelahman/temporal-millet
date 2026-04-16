module Error = Utils.Error
module Backend = CliInterpreter.Make (Language.Tau.IntervalTau)
module Tau = Backend.Tau
module PrettyPrint = Language.PrettyPrint
module Loader = Loader.Loader (Backend)

type config = { filenames : string list; use_stdlib : bool; debug : bool }

let parse_args_to_config () =
  let filenames = ref [] and use_stdlib = ref true and debug = ref false in
  let usage = "Run Millet as '" ^ Sys.argv.(0) ^ " [filename.mlt] ...'"
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
      ]
  in
  Arg.parse options anonymous usage;
  { filenames = List.rev !filenames; use_stdlib = !use_stdlib; debug = !debug }

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

let main () =
  let config = parse_args_to_config () in
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

let _ = main ()
