module Location = Utils.Location
module Error = Utils.Error
module Diagnostic = Utils.Diagnostic
module List = Utils.List
module Ast = Language.Ast
open Backend

module Loader (Backend : Backend.S) = struct
  module D = Desugarer.Make (Backend.ResourceGrade)
  module TC = Typechecker.Make (Backend.ResourceGrade)
  module G = Parser.Grammar.Make (Backend.ResourceGrade)

  type state = {
    desugarer : D.state;
    backend : Backend.load_state;
    typechecker : TC.state;
  }

  let load_primitive state prim =
    let x = Ast.Variable.fresh (Language.Primitives.primitive_name prim) in
    let desugarer_state' = D.load_primitive state.desugarer x prim in
    let typechecker_state' = TC.load_primitive state.typechecker x prim in
    let backend_state' = Backend.load_primitive state.backend x prim in
    {
      desugarer = desugarer_state';
      typechecker = typechecker_state';
      backend = backend_state';
    }

  let initial_state =
    let initial_state_without_primitives =
      {
        desugarer = D.initial_state;
        typechecker = TC.initial_state;
        backend = Backend.initial_load_state;
      }
    in

    List.fold_left load_primitive initial_state_without_primitives
      Language.Primitives.primitives

  let parse_commands lexbuf =
    try G.commands Parser.Lexer.token lexbuf with
    | G.Error -> Error.syntax ~loc:(Location.of_lexbuf lexbuf) "parser error"
    | Failure failmsg when failmsg = "lexing: empty token" ->
        Error.syntax ~loc:(Location.of_lexbuf lexbuf) "unrecognised symbol"
    (* Grade literals are converted by the grading monoid the parser is
       parameterised by, which rejects literal forms it does not support. This
       is the usual symptom of running a file under the wrong grading monoid,
       so report it as a located syntax error naming the monoid in use. *)
    | Invalid_argument msg ->
        Error.syntax
          ~loc:(Location.of_lexbuf lexbuf)
          "in the '%s' grading monoid, %s" Backend.ResourceGrade.name msg

  (* A typing error raised without a location of its own is pinned to the
     command being executed. *)
  let execute_command state (cmd : _ Ast.command) =
    try
      match cmd.it with
      | Ast.TyDef (eternality, ty_defs) ->
          let typechecker_state' =
            TC.add_type_definitions ~loc:cmd.at state.typechecker
              (eternality, ty_defs)
          in
          let backend_state' =
            Backend.load_ty_def state.backend (eternality, ty_defs)
          in
          {
            state with
            typechecker = typechecker_state';
            backend = backend_state';
          }
      | Ast.OpSig (op, ty1, ty2, rho, bounds) ->
          let typechecker_state' =
            TC.add_operation_signature ~loc:cmd.at state.typechecker
              (op, ty1, ty2, rho, bounds)
          in
          let _evaluation_environment_state' = state.backend in
          {
            state with
            typechecker = typechecker_state';
            backend = Backend.load_op_sig state.backend op rho;
          }
      | Ast.OpDefault (op, abs) ->
          let typechecker_state' =
            TC.add_operation_default ~loc:cmd.at state.typechecker (op, abs)
          in
          {
            state with
            typechecker = typechecker_state';
            backend = Backend.load_op_default state.backend op abs;
          }
      | Ast.TopLet (x, expr) ->
          let typechecker_state' =
            TC.add_top_definition ~loc:cmd.at state.typechecker x expr
          in
          let backend_state' = Backend.load_top_let state.backend x expr in
          {
            state with
            typechecker = typechecker_state';
            backend = backend_state';
          }
      | Ast.TopDo comp ->
          let _ = TC.infer ~loc:cmd.at state.typechecker comp in
          let backend_state' = Backend.load_top_do state.backend comp in
          { state with backend = backend_state' }
    with Error.Error ({ primary = None; _ } as d) ->
      raise (Error.Error { d with primary = Some cmd.at })

  (* What is left to check after a command was rejected. [Stop] gives up: the
     rest could only repeat that a type or operation was never declared. *)
  type recovery = Continue of state | Stop

  let recover_from state (cmd : _ Ast.command) =
    match cmd.it with
    (* The definition is assumed to have any type at all, so that its uses below
       are checked rather than reported as unknown variables. The backend is left
       alone: nothing is run once there are errors. *)
    | Ast.TopLet (x, _) ->
        Continue
          {
            state with
            typechecker = TC.assume_definition ~loc:cmd.at state.typechecker x;
          }
    (* A [run] exports nothing and a rejected default leaves the operation
       without one, so the next command is checked in the state before it. *)
    | Ast.TopDo _ | Ast.OpDefault _ -> Continue state
    | Ast.TyDef _ | Ast.OpSig _ -> Stop

  (** Execute [cmds] in order, returning the state they leave behind and the
      diagnostics they produced. With [~recover:true] the typing errors the
      loader can carry on from are collected, so that a program's independent
      errors are reported at once; otherwise the first escapes as an exception.
      Errors of any other kind are fatal either way. *)
  let execute_commands ~recover state cmds =
    let step (state, diagnostics, stopped) cmd =
      if stopped then (state, diagnostics, stopped)
      else
        match execute_command state cmd with
        | state' -> (state', diagnostics, false)
        | exception Error.Error ({ kind = Diagnostic.Typing; _ } as d)
          when recover -> (
            match recover_from state cmd with
            | Continue state' -> (state', d :: diagnostics, false)
            | Stop -> (state, d :: diagnostics, true))
    in
    let state', diagnostics, _ = List.fold_left step (state, [], false) cmds in
    (state', List.rev diagnostics)

  let desugar_commands state cmds =
    let desugarer_state', cmds' =
      List.fold_map D.desugar_command state.desugarer cmds
    in
    ({ state with desugarer = desugarer_state' }, cmds')

  let load_commands_all state cmds =
    let state', cmds' = desugar_commands state cmds in
    execute_commands ~recover:true state' cmds'

  (* Without recovery the first error escapes as an exception and the
     diagnostic list is empty; re-raising covers the case all the same. *)
  let load_commands state cmds =
    let state', cmds' = desugar_commands state cmds in
    match execute_commands ~recover:false state' cmds' with
    | state'', [] -> state''
    | _, d :: _ -> raise (Error.Error d)

  let parse_source ?(filename = "") source =
    let lexbuf = Lexing.from_string source in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    parse_commands lexbuf

  let load_source ?filename state source =
    load_commands state (parse_source ?filename source)

  let load_file state source =
    load_commands state (Parser.Lexer.read_file parse_commands source)

  (** Load a source, reporting every typing error it contains rather than only
      the first. Parsing and desugaring stay fatal: a parse error leaves nothing
      to continue with, and an unknown name would only cascade. The standard
      library goes through {!load_source}, an error in it being a bug. *)
  let load_source_all ?filename state source =
    load_commands_all state (parse_source ?filename source)

  (** {!load_source_all} for a file. *)
  let load_file_all state source =
    load_commands_all state (Parser.Lexer.read_file parse_commands source)

  (** The module Stdlib_mlt is automatically generated from stdlib.mlt. Check
      the dune file for details. *)
  let stdlib_source = Stdlib_mlt.contents
end

(** The standard library's source, independently of any backend, for callers
    that need to know what precedes a program's own source. *)
let stdlib_source = Stdlib_mlt.contents

(** The file name the standard library's locations are reported under, since it
    is loaded from a string rather than from a file. *)
let stdlib_filename = "stdlib.mlt"
