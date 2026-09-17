module Error = Utils.Error
module Diagnostic = Utils.Diagnostic
module Location = Utils.Location

(* Abstract representation of a single reduction step, with all resource-grade-specific
   types captured in closures. This allows the model to work with any resource grade
   without fixing the type at module-definition time. *)
type concrete_step = {
  label_vdom : msg Vdom.vdom;
      (** Pre-rendered step label. Produces [msg] events (in practice none,
          since [view_step_label] is parametrically polymorphic and event-free).
      *)
  view_highlighted : 'a. unit -> 'a Vdom.vdom;
      (** Render the current run state with this step's redex highlighted. *)
  next_state : unit -> run_model_state;
      (** Advance to the next run state by performing this step. *)
}

and completed_run_view = { view_completed : 'a. unit -> 'a Vdom.vdom }

and run_model_state = {
  steps : concrete_step list;
  view : 'a. unit -> 'a Vdom.vdom;
      (** Render the current run state with no redex highlighted. *)
  completed_runs : completed_run_view list;
      (** Snapshots of previously finished [run] blocks, in chronological order.
          Each snapshot is the view of the run-state at the moment its
          terminating [Return] step was about to be taken. *)
  is_done : bool;
      (** True when there are no computations left to run (i.e. all [run] blocks
          have completed and their snapshots are in [completed_runs]). *)
}
(** A snapshot of an interpreter run state together with its available steps,
    all resource-grade types hidden behind closures. *)

and edit_msg =
  | UseStdlib of bool
  | ChangeSource of string
  | InsertIndent of string * int * int
      (** Tab pressed in the editor: the source as the browser has it, and the
          selection to replace with an indentation, in the UTF-16 code units the
          browser counts selections in. *)
  | LoadExample of string * string * string
      (** Load a bundled example: its title, the name of the resource grade it
          is meant to be run with, and its source. *)
  | SelectResource of string
      (** Select the resource grade to use (by name from
          [resource_grade_modules]). *)

and run_msg =
  | SelectStepIndex of int option
  | MakeStep of concrete_step
  | RandomStep
  | ChangeRandomStepSize of int
  | Back

and msg =
  | EditMsg of edit_msg
  | RunCode
  | RunMsg of run_msg
  | EditCode
  | HoverLabel of int option
      (** The pointer has entered the given label of a reported error, or left
          them. Top-level, since the error display belongs to neither the editor
          nor the run. *)
  | HoverError of int option
      (** The pointer has entered the given reported error — any of its line
          numbers in the editor, its entry in the side panel, or the header of
          its message — or left it. *)
  | CaretAt of int
      (** The caret has been placed at the given offset of the editor, in UTF-16
          code units: the error whose span it lands in, if any, becomes the
          active one. *)

type edit_model = {
  use_stdlib : bool;
  unparsed_code : string;
  selected_resource : string;
      (** Name of the currently selected resource grade (key in
          [resource_grade_modules]). *)
  selected_example : string option;
      (** Title of the bundled example currently loaded, if the source has not
          been edited since. *)
}

let default_resource_name =
  fst (List.hd Language.ResourceGrade.resource_grade_modules)

let edit_init =
  {
    use_stdlib = true;
    unparsed_code = "";
    selected_resource = default_resource_name;
    selected_example = None;
  }

(** What a Tab in the editor inserts; the editor's [tab-size] matches. *)
let indentation = "  "

(** [byte_offset source offset] is the byte of [source] the browser's [offset]
    points at: a selection counts UTF-16 code units, an OCaml string UTF-8
    bytes, and the two part company outside ASCII. *)
let byte_offset source offset =
  let length = String.length source in
  let rec go byte units =
    if units >= offset || byte >= length then byte
    else
      let lead = Char.code source.[byte] in
      let width =
        if lead < 0x80 then 1
        else if lead < 0xE0 then 2
        else if lead < 0xF0 then 3
        else 4
      in
      (* outside the basic plane: a surrogate pair, so two code units *)
      go (byte + width) (units + if width = 4 then 2 else 1)
  in
  go 0 0

let edit_update edit_model = function
  | UseStdlib use_stdlib -> { edit_model with use_stdlib }
  | ChangeSource input ->
      { edit_model with unparsed_code = input; selected_example = None }
  | InsertIndent (source, start, stop) ->
      let start = byte_offset source start and stop = byte_offset source stop in
      let before = String.sub source 0 start
      and after = String.sub source stop (String.length source - stop) in
      {
        edit_model with
        unparsed_code = before ^ indentation ^ after;
        selected_example = None;
      }
  | LoadExample (title, resource_name, source) ->
      (* An example is written for a particular resource grade, so loading one
         switches to that grade. The user remains free to change it afterwards. *)
      {
        edit_model with
        unparsed_code = source;
        selected_resource = resource_name;
        selected_example = Some title;
      }
  | SelectResource name -> { edit_model with selected_resource = name }

type run_model = {
  current : run_model_state;
  history : run_model_state list;
  selected_step_index : int option;
  (* You may be wondering why we keep an index rather than the selected step itself.
     The selected step is displayed when the user moves the mouse over the step button,
     so on a onmouseover event. However, in the common case, when the user is on the button
     and is clicking it to proceed, this event is not triggered and so the step is not updated.
     For that reason, it is easiest to keep track of the selected button index, which does not
     change when the user clicks the button. *)
  random_step_size : int;
}

let run_init current =
  { current; history = []; selected_step_index = None; random_step_size = 1 }

let run_model_make_step run_model (step : concrete_step) =
  {
    run_model with
    current = step.next_state ();
    history = run_model.current :: run_model.history;
  }

let rec run_model_make_random_steps run_model num_steps =
  match (num_steps, run_model.current.steps) with
  | 0, _ | _, [] -> run_model
  | _, steps ->
      let i = Random.int (List.length steps) in
      let step = List.nth steps i in
      let run_model' = run_model_make_step run_model step in
      run_model_make_random_steps run_model' (num_steps - 1)

let run_update run_model = function
  | SelectStepIndex selected_step_index ->
      { run_model with selected_step_index }
  | MakeStep step -> run_model_make_step run_model step
  | RandomStep ->
      run_model_make_random_steps run_model run_model.random_step_size
  | Back -> (
      match run_model.history with
      | current' :: history' ->
          { run_model with current = current'; history = history' }
      | _ -> run_model)
  | ChangeRandomStepSize random_step_size -> { run_model with random_step_size }

type load_error = {
  diagnostic : Diagnostic.t;  (** why the source could not be loaded and run *)
  hovered_label : int option;
      (** The label the pointer is over, whose span is brightened in the editor.
      *)
}
(** An error the edit view reports, with the state of showing it. *)

type model = {
  edit_model : edit_model;
  run_model : (run_model, load_error list) result;
      (** [Error []] is the edit view with nothing to report. *)
  active_error : int option;
      (** The error the caret sits in, singled out among the messages. *)
  hovered_error : int option;
      (** The error the pointer is on, all of whose line numbers light up
          together. Kept here rather than left to CSS, the numbers of one error
          being separate elements with no parent of their own. *)
  stale_errors : bool;
      (** Whether the source has been edited since the errors were reported, so
          that their spans point at bytes that have moved. *)
}

let init =
  {
    edit_model = edit_init;
    run_model = Error [];
    active_error = None;
    hovered_error = None;
    stale_errors = false;
  }

(* An error that is not a diagnostic of its own, such as an exception escaping
   the interpreter: there is nothing to point at, only what went wrong. *)
let fatal message =
  {
    diagnostic =
      {
        Diagnostic.kind = Fatal;
        primary = None;
        message;
        labels = [];
        notes = [];
      };
    hovered_label = None;
  }

(* Whether an edit moves the text that the reported errors point into. *)
let edits_source = function
  | ChangeSource _ | InsertIndent _ | LoadExample _ -> true
  | UseStdlib _ | SelectResource _ -> false

(* The first error whose primary span covers [offset], a byte of the editor's
   text. A point span is widened as the editor widens it when marking it. *)
let error_at errors offset =
  let covers (error : load_error) =
    match error.diagnostic.primary with
    | Some { filename = ""; start; stop } ->
        start.offset <= offset && offset <= max stop.offset (start.offset + 1)
    | _ -> false
  in
  List.find_index covers errors

let update model = function
  | EditMsg edit_msg ->
      {
        model with
        edit_model = edit_update model.edit_model edit_msg;
        active_error = None;
        hovered_error = None;
        stale_errors = model.stale_errors || edits_source edit_msg;
      }
  | RunMsg run_msg -> (
      match model.run_model with
      | Ok run_model ->
          { model with run_model = Ok (run_update run_model run_msg) }
      | Error _ -> model)
  | RunCode ->
      let run_model =
        try
          match
            List.assoc_opt model.edit_model.selected_resource
              Language.ResourceGrade.resource_grade_modules
          with
          | None ->
              Error
                [
                  fatal
                    (Printf.sprintf "Unknown resource grade '%s'"
                       model.edit_model.selected_resource);
                ]
          | Some (module RG : Language.ResourceGrade.Grade) ->
              let module B = WebInterpreter.Make (RG) in
              let module L = Loader.Loader (B) in
              (* Loaded as two separate sources, so that an editor location
                 is a location in what the user typed. *)
              let state =
                if model.edit_model.use_stdlib then
                  L.load_source ~filename:Loader.stdlib_filename L.initial_state
                    L.stdlib_source
                else L.initial_state
              in
              let state, diagnostics =
                L.load_source_all state model.edit_model.unparsed_code
              in
              (* Build a run_model_state from a B.run_state, capturing all
                 resource-grade-specific types in closures so the rest of the
                 application is independent of the chosen resource grade. *)
              let rec make_run_state ~completed_runs (rs : B.run_state) :
                  run_model_state =
                {
                  steps =
                    List.map
                      (fun (step : B.step) ->
                        let next_completed_runs =
                          if B.is_return_label step.label then
                            completed_runs
                            @ [
                                {
                                  view_completed =
                                    (fun () -> B.view_run_state rs None);
                                };
                              ]
                          else completed_runs
                        in
                        {
                          label_vdom =
                            (B.view_step_label step.label : msg Vdom.vdom);
                          view_highlighted =
                            (fun () -> B.view_run_state rs (Some step.label));
                          next_state =
                            (fun () ->
                              make_run_state ~completed_runs:next_completed_runs
                                (step.next_state ()));
                        })
                      (B.steps rs);
                  view = (fun () -> B.view_run_state rs None);
                  completed_runs;
                  is_done = B.is_done rs;
                }
              in
              if diagnostics <> [] then
                Error
                  (List.map
                     (fun diagnostic -> { diagnostic; hovered_label = None })
                     diagnostics)
              else
                Ok
                  (run_init
                     (make_run_state ~completed_runs:[] (B.run state.backend)))
        with
        | Error.Error d -> Error [ { diagnostic = d; hovered_label = None } ]
        | Invalid_argument message -> Error [ fatal message ]
        | exn -> Error [ fatal (Printexc.to_string exn) ]
      in
      {
        model with
        run_model;
        active_error = None;
        hovered_error = None;
        stale_errors = false;
      }
  | EditCode ->
      {
        model with
        run_model = Error [];
        active_error = None;
        hovered_error = None;
        stale_errors = false;
      }
  | HoverLabel hovered_label -> (
      match model.run_model with
      | Error errors ->
          let errors =
            List.map (fun error -> { error with hovered_label }) errors
          in
          { model with run_model = Error errors }
      | Ok _ -> model)
  | HoverError hovered_error -> { model with hovered_error }
  | CaretAt offset -> (
      (* Edited source: the spans no longer say where the caret is. *)
      match model.run_model with
      | Error errors when not model.stale_errors ->
          let offset = byte_offset model.edit_model.unparsed_code offset in
          { model with active_error = error_at errors offset }
      | _ -> model)
