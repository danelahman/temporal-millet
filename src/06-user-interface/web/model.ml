module Error = Utils.Error

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

and run_model_state = {
  steps : concrete_step list;
  view : 'a. unit -> 'a Vdom.vdom;
      (** Render the current run state with no redex highlighted. *)
}
(** A snapshot of an interpreter run state together with its available steps,
    all resource-grade types hidden behind closures. *)

and edit_msg =
  | UseStdlib of bool
  | ChangeSource of string
  | SelectResource of string
      (** Select the resource grade to use (by name from
          [resource_grade_modules]). *)

and run_msg =
  | SelectStepIndex of int option
  | MakeStep of concrete_step
  | RandomStep
  | ChangeRandomStepSize of int
  | Back

and msg = EditMsg of edit_msg | RunCode | RunMsg of run_msg | EditCode

type edit_model = {
  use_stdlib : bool;
  unparsed_code : string;
  selected_resource : string;
      (** Name of the currently selected resource grade (key in
          [resource_grade_modules]). *)
}

let default_resource_name =
  fst (List.hd Language.ResourceGrade.resource_grade_modules)

let edit_init =
  {
    use_stdlib = true;
    unparsed_code = "";
    selected_resource = default_resource_name;
  }

let edit_update edit_model = function
  | UseStdlib use_stdlib -> { edit_model with use_stdlib }
  | ChangeSource input -> { edit_model with unparsed_code = input }
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

type model = { edit_model : edit_model; run_model : (run_model, string) result }

let init = { edit_model = edit_init; run_model = Error "" }

let update model = function
  | EditMsg edit_msg ->
      { model with edit_model = edit_update model.edit_model edit_msg }
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
                (Printf.sprintf "Unknown resource grade '%s'"
                   model.edit_model.selected_resource)
          | Some (module RG : Language.ResourceGrade.S) ->
              let module B = WebInterpreter.Make (RG) in
              let module L = Loader.Loader (B) in
              let source =
                (if model.edit_model.use_stdlib then L.stdlib_source else "")
                ^ "\n\n\n" ^ model.edit_model.unparsed_code
              in
              let state = L.load_source L.initial_state source in
              let run_state = B.run state.backend in
              (* Build a run_model_state from a B.run_state, capturing all
                 resource-grade-specific types in closures so the rest of the
                 application is independent of the chosen resource grade. *)
              let rec make_run_state (rs : B.run_state) : run_model_state =
                {
                  steps =
                    List.map
                      (fun (step : B.step) ->
                        {
                          label_vdom =
                            (B.view_step_label step.label : msg Vdom.vdom);
                          view_highlighted =
                            (fun () -> B.view_run_state rs (Some step.label));
                          next_state =
                            (fun () -> make_run_state (step.next_state ()));
                        })
                      (B.steps rs);
                  view = (fun () -> B.view_run_state rs None);
                }
              in
              Ok (run_init (make_run_state run_state))
        with Error.Error (_, _, msg) -> Error msg
      in
      { model with run_model }
  | EditCode -> { model with run_model = Error "" }
