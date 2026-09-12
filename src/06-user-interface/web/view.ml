open Vdom
module Ast = Language.Ast
module SyntaxHighlight = WebInterpreter.SyntaxHighlight

(* Auxiliary definitions *)
let panel ?(a = []) heading blocks =
  div ~a:(class_ "panel" :: a)
    (elt "p" ~a:[ class_ "panel-heading" ] [ text heading ] :: blocks)

let panel_block = div ~a:[ class_ "panel-block" ]

let button txt msg =
  input [] ~a:[ onclick (fun _ -> msg); type_button; value txt ]

let disabled_button txt = input [] ~a:[ type_button; value txt; disabled true ]

let select ?(a = []) empty_description msg describe_choice selected choices =
  let view_choice choice =
    elt "option"
      ~a:[ bool_prop "selected" (selected choice) ]
      [ text (describe_choice choice) ]
  in
  div ~a
    [
      elt "select"
        ~a:[ onchange_index (fun i -> msg (List.nth choices (i - 1))) ]
        (elt "option"
           ~a:
             [
               disabled true;
               bool_prop "selected"
                 (List.for_all (fun choice -> not (selected choice)) choices);
             ]
           [ text empty_description ]
        :: List.map view_choice choices);
    ]

let nil = text ""

let view_contents main aside =
  div
    ~a:[ class_ "contents columns" ]
    [
      div ~a:[ class_ "main column is-three-quarters" ] main;
      div ~a:[ class_ "aside column is-one-quarter" ] aside;
    ]

(* Edit view *)

let view_editor (model : Model.edit_model) =
  let rows =
    max 10 (String.split_on_char '\n' model.unparsed_code |> List.length)
  in
  let highlighted =
    SyntaxHighlight.highlight_text (model.unparsed_code ^ "\n")
  in
  div
    ~a:[ class_ "box" ]
    [
      div
        ~a:[ class_ "code-editor" ]
        [
          elt "pre" ~a:[ class_ "code-editor-display syn-ml" ] highlighted;
          elt "textarea"
            ~a:
              [
                class_ "code-editor-input";
                oninput (fun input -> Model.ChangeSource input);
                int_prop "rows" rows;
                attr "spellcheck" "false";
                attr "autocapitalize" "off";
                attr "autocorrect" "off";
              ]
            [ text model.unparsed_code ];
        ];
    ]

(* let _view (model : Model.model) =
   match model.loaded_code with
   | Ok code ->
       div
         [
           input ~a:[type_ "range"; int_attr "min" 0; int_attr "max" 10; int_attr "step" 2; onmousedown (fun event -> Model.ParseInterrupt (string_of_int event.x))] [];
           (* elt "progress" ~a:[type_ "range"; value (string_of_int model.random_step_size); int_attr "max" 10; oninput (fun input -> Model.ChangeStepSize input)] []; *)
           editor model;
           actions model code;
           view_operations code.snapshot.operations;
           view_process code.snapshot.process;
         ]
   | Error msg -> div [ editor model; text msg ] *)

let view_compiler (model : Model.model) =
  let use_stdlib =
    elt "label"
      ~a:[ class_ "panel-block" ]
      [
        input
          ~a:
            [
              type_ "checkbox";
              onchange_checked (fun use_stdlib ->
                  Model.EditMsg (Model.UseStdlib use_stdlib));
              bool_prop "checked" model.edit_model.use_stdlib;
            ]
          [];
        text "Load standard library";
      ]
  in
  let load_example =
    div
      ~a:[ class_ "panel-block" ]
      [
        div
          ~a:[ class_ "field" ]
          [
            elt "label" ~a:[ class_ "label" ] [ text "Example" ];
            div
              ~a:[ class_ "control is-expanded" ]
              [
                select
                  ~a:[ class_ "select is-fullwidth" ]
                  "Load example"
                  (fun (title, resource_name, source) ->
                    Model.EditMsg (LoadExample (title, resource_name, source)))
                  (fun (title, _, _) -> title)
                  (fun (title, _, _) ->
                    Some title = model.edit_model.selected_example)
                  (* The module Examples_mlt is semi-automatically generated from examples/*.mlt. Check the dune file for details. *)
                  Examples_mlt.examples;
              ];
          ];
      ]
  and select_resource =
    div
      ~a:[ class_ "panel-block" ]
      [
        div
          ~a:[ class_ "field" ]
          [
            elt "label" ~a:[ class_ "label" ] [ text "Resource grade" ];
            div
              ~a:[ class_ "control is-expanded" ]
              [
                select
                  ~a:[ class_ "select is-fullwidth" ]
                  "Select resource grade"
                  (fun name -> Model.EditMsg (Model.SelectResource name))
                  (fun name -> name)
                  (fun name -> name = model.edit_model.selected_resource)
                  (List.map fst Language.ResourceGrade.resource_grade_modules);
              ];
          ];
      ]
  and run_process =
    panel_block
      [
        elt "button"
          ~a:
            [
              class_ "button is-info is-fullwidth";
              onclick (fun _ -> Model.RunCode);
              (* disabled (Result.is_error model.loaded_code); *)
            ]
          [ text "Typecheck & run" ];
        (match model.run_model with
        | Error msg -> elt "p" ~a:[ class_ "help is-danger" ] [ text msg ]
        | Ok _ -> nil);
      ]
  in
  panel "Code options"
    [ use_stdlib; load_example; select_resource; run_process ]

let edit_view (model : Model.model) =
  view_contents
    [
      map
        (fun edit_msg -> Model.EditMsg edit_msg)
        (view_editor model.edit_model);
    ]
    [ view_compiler model ]

(* Run view *)

let view_steps (run_model : Model.run_model) steps =
  let view_edit_source =
    panel_block
      [
        elt "button"
          ~a:
            [
              class_ "button is-outlined is-fullwidth is-small is-danger";
              onclick (fun _ -> Model.EditCode);
              attr "title"
                "Re-editing source code will abort current evaluation";
            ]
          [ text "Re-edit source code" ];
      ]
  and view_undo_last_step =
    panel_block
      [
        elt "button"
          ~a:
            [
              class_ "button is-outlined is-fullwidth is-small";
              onclick (fun _ -> Model.RunMsg Model.Back);
              disabled (run_model.history = []);
            ]
          [ text "Undo last step" ];
      ]
  and view_step i step =
    panel_block
      [
        elt "button"
          ~a:
            [
              class_ "button is-outlined is-fullwidth";
              onclick (fun _ -> Model.RunMsg (Model.MakeStep step));
              onmouseenter (fun _ ->
                  Model.RunMsg (Model.SelectStepIndex (Some i)));
            ]
          [ step.Model.label_vdom ];
      ]
  and view_random_steps steps =
    div
      ~a:[ class_ "panel-block" ]
      [
        div
          ~a:[ class_ "field has-addons" ]
          [
            div
              ~a:[ class_ "control is-expanded" ]
              [
                select
                  ~a:[ class_ "select is-fullwidth is-info" ]
                  "Step size"
                  (fun step_size ->
                    Model.RunMsg (Model.ChangeRandomStepSize step_size))
                  string_of_int
                  (fun step_size -> step_size = run_model.random_step_size)
                  [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512; 1024 ];
              ];
            div
              ~a:[ class_ "control" ]
              [
                elt "button"
                  ~a:
                    [
                      class_ "button is-info";
                      onclick (fun _ -> Model.RunMsg Model.RandomStep);
                      disabled (steps = []);
                    ]
                  [ text "random steps" ];
              ];
          ];
        (if steps = [] then
           elt "p"
             ~a:[ class_ "help" ]
             [
               text "Computation has terminated, no further steps are possible.";
             ]
         else text "");
      ]
  in
  panel "Interaction"
    ~a:[ onmouseleave (fun _ -> Model.RunMsg (Model.SelectStepIndex None)) ]
    (view_edit_source :: view_undo_last_step :: view_random_steps steps
   :: List.mapi view_step steps)

let run_view (run_model : Model.run_model) =
  let steps = run_model.current.steps in
  let selected_step =
    Option.map (List.nth steps) run_model.selected_step_index
  in
  let active_view =
    if run_model.current.is_done && run_model.current.completed_runs <> [] then
      []
    else
      let state_view =
        match selected_step with
        | None -> run_model.current.view ()
        | Some step -> step.view_highlighted ()
      in
      [ state_view ]
  in
  let completed_views =
    List.concat_map
      (fun (cr : Model.completed_run_view) ->
        [
          div
            ~a:[ class_ "completed-run-separator" ]
            [ elt "span" [ text "previous run" ] ];
          cr.view_completed ();
        ])
      run_model.current.completed_runs
  in
  view_contents (active_view @ completed_views) [ view_steps run_model steps ]

let github_url = "https://github.com/danelahman/temporal-millet"

(* The GitHub mark, as in GitHub's Octicons (MIT licensed), drawn in the
   current text colour so that it follows the link's hover styling. *)
let github_mark =
  svg_elt "svg"
    ~a:
      [
        attr "viewBox" "0 0 16 16";
        attr "width" "20";
        attr "height" "20";
        attr "aria-hidden" "true";
      ]
    [
      svg_elt "path"
        ~a:
          [
            attr "fill" "currentColor";
            attr "d"
              "M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 \
               7.59.4.07.55-.17.55-.38 \
               0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 \
               1.08.58 1.23.82.72 1.21 1.87.87 \
               2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 \
               0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 \
               2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 \
               2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 \
               3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 \
               1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 \
               8c0-4.42-3.58-8-8-8z";
          ]
        [];
    ]

let view_navbar =
  let view_title =
    div
      ~a:[ class_ "navbar-brand" ]
      [
        elt "a"
          ~a:[ class_ "navbar-item" ]
          [ elt "p" ~a:[ class_ "title" ] [ text "Temporal Millet" ] ];
        elt "a"
          ~a:
            [
              class_ "navbar-item github-link";
              attr "href" github_url;
              attr "target" "_blank";
              attr "rel" "noopener";
              attr "title" "Source code on GitHub";
            ]
          [
            elt "span" ~a:[ class_ "icon" ] [ github_mark ];
            elt "span" [ text "GitHub" ];
          ];
      ]
  in

  elt "navbar" ~a:[ class_ "navbar" ] [ view_title ]

let view (model : Model.model) =
  div
    [
      view_navbar;
      (match model.run_model with
      | Error _ -> edit_view model
      | Ok run_model -> run_view run_model);
    ]
