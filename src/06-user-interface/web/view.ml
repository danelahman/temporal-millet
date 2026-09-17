open Vdom
module Ast = Language.Ast
module Diagnostic = Utils.Diagnostic
module Location = Utils.Location
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
      (* index 0 is the placeholder below, and a browser may report it *)
      elt "select"
        ~a:
          [
            on "change"
              Vdom.Decoder.(
                map
                  (fun i -> Option.map msg (List.nth_opt choices (i - 1)))
                  (field "target.selectedIndex" Int));
          ]
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

(* An error that stopped the program from being loaded is highlighted in the
   editor and explained under it, in the colour of a Bulma "danger" message. *)

(* The standard library is loaded as a source of its own, under a file name,
   and is neither shown nor editable; only editor spans can be marked. *)
let in_editor (loc : Location.t) = loc.filename = ""

(* The ids the error's spans are given in the editor, so that the message
   under it can link to them and the page can scroll to them. *)
let primary_id i = Printf.sprintf "error-primary-%d" i
let label_id i j = Printf.sprintf "error-label-%d-%d" i j
let error_id i = Printf.sprintf "editor-error-%d" i

(* The kind of error and where it points: an editor span by its line and
   characters, a standard-library span by its file as well. *)
let load_error_header (error : Model.load_error) =
  let kind = Diagnostic.kind_to_string error.diagnostic.kind in
  match error.diagnostic.primary with
  | None -> kind
  | Some loc when in_editor loc ->
      Format.asprintf "%s at %t" kind (Location.print_short loc)
  | Some loc ->
      Format.asprintf "%s in the standard library (%t)" kind
        (Location.print loc)

(* The same, cut down for the side panel's list: no characters, no file. *)
let load_error_summary (error : Model.load_error) =
  let kind = Diagnostic.kind_to_string error.diagnostic.kind in
  match error.diagnostic.primary with
  | None -> kind
  | Some loc when in_editor loc ->
      Printf.sprintf "%s at line %d" kind loc.start.line
  | Some _ -> kind ^ " in the standard library"

(* The line numbers of one error are separate elements, so they are lit as a
   block through the model: each reports the pointer entering or leaving, and
   wears [is-hover] while the model says this error is the one under it. *)
let error_number_attrs ~hovered i =
  (if hovered then [ class_ "is-hover" ] else [])
  @ [
      onmouseenter (fun _ -> Model.HoverError (Some i));
      onmouseleave (fun _ -> Model.HoverError None);
    ]

(* What the [i]th error marks in the editor: its primary span, whose lines the
   gutter numbers in red, and each of its labels, the hovered one brightened.
   Standard-library spans mark nothing. *)
let marks_of_error ~hovered i (error : Model.load_error) =
  let mark ?marker mark_cls id (loc : Location.t) =
    if in_editor loc then
      [
        {
          SyntaxHighlight.from = loc.start.offset;
          until = loc.stop.offset;
          mark_cls;
          id = Some id;
          marker;
        };
      ]
    else []
  in
  (match error.diagnostic.primary with
    | Some loc ->
        mark "error-primary" (primary_id i) loc
          ~marker:
            {
              SyntaxHighlight.href = "#" ^ error_id i;
              title = load_error_header error;
              attrs = error_number_attrs ~hovered i;
            }
    | None -> [])
  @ List.concat
      (List.mapi
         (fun j ({ span; _ } : Diagnostic.label) ->
           let mark_cls =
             if error.hovered_label = Some j then "error-related error-hover"
             else "error-related"
           in
           mark mark_cls (label_id i j) span)
         error.diagnostic.labels)

(* What to scroll to for an error: its highlighted primary span when it has
   one in the editor, otherwise the message block under the editor. *)
let load_error_target i (error : Model.load_error) =
  match error.diagnostic.primary with
  | Some loc when in_editor loc -> primary_id i
  | _ -> error_id i

(* One error's message. [active] is the error the caret sits in; under [stale]
   the editor marks nothing, so the links into it are left out. *)
let view_load_error ~stale ~active i (error : Model.load_error) =
  (* In the editor a label links to its span, which lights up while the
     pointer is on it; a standard-library span can only be named. *)
  let view_label j ({ span; text = label_text } : Diagnostic.label) =
    let where =
      elt "span"
        ~a:[ class_ "error-line-ref" ]
        [
          text
            (if in_editor span then Printf.sprintf " (line %d)" span.start.line
             else Printf.sprintf " (standard library, line %d)" span.start.line);
        ]
    in
    if in_editor span && not stale then
      elt "li"
        ~a:[ onmouseenter (fun _ -> Model.HoverLabel (Some j)) ]
        [
          elt "a"
            ~a:[ attr "href" ("#" ^ label_id i j) ]
            [ text label_text; where ];
        ]
    else elt "li" [ text label_text; where ]
  in
  let labels =
    match error.diagnostic.labels with
    | [] -> []
    | labels ->
        [
          elt "ul"
            ~a:
              [
                class_ "error-labels";
                onmouseleave (fun _ -> Model.HoverLabel None);
              ]
            (List.mapi view_label labels);
        ]
  and notes =
    match error.diagnostic.notes with
    | [] -> []
    | notes ->
        [
          elt "ul"
            ~a:[ class_ "error-notes" ]
            (List.map (fun note -> elt "li" [ text note ]) notes);
        ]
  in
  let header_aside =
    if stale then elt "span" ~a:[ class_ "stale-note" ] [ text "out of date" ]
    else
      match error.diagnostic.primary with
      | Some loc when in_editor loc ->
          elt "a"
            ~a:[ class_ "show-in-editor"; attr "href" ("#" ^ primary_id i) ]
            [ text "show in editor" ]
      | _ -> nil
  in
  let classes =
    String.concat " "
      ([ "message"; "is-danger"; "editor-error" ]
      @ (if active then [ "is-active" ] else [])
      @ if stale then [ "is-stale" ] else [])
  in
  elt "article"
    ~a:[ class_ classes; attr "id" (error_id i) ]
    [
      div
        ~a:
          [
            class_ "message-header";
            onmouseenter (fun _ -> Model.HoverError (Some i));
            onmouseleave (fun _ -> Model.HoverError None);
          ]
        [
          elt "p"
            [
              text
                (Printf.sprintf "Error %d — %s" (i + 1)
                   (load_error_header error));
            ];
          header_aside;
        ];
      div
        ~a:[ class_ "message-body" ]
        ((elt "p" [ text error.diagnostic.message ] :: labels) @ notes);
    ]

(* Tab inside the editor inserts an indentation instead of moving the focus
   to the next control; Shift+Tab is left alone, so the keyboard can still
   leave the editor backwards. The browser's own value and selection are read
   off the event, since the model may lag behind a fast typist. *)
let oninsert_indent =
  let open Vdom.Decoder in
  let target d = field "target" d in
  on_with_options "keydown"
    (bind
       (fun (key, shift) ->
         if key = "Tab" && not shift then
           app
             (app
                (app
                   (const (fun source start stop ->
                        {
                          Vdom.msg =
                            Some
                              (Model.EditMsg
                                 (Model.InsertIndent (source, start, stop)));
                          prevent_default = true;
                          stop_propagation = false;
                        }))
                   (target (field "value" String)))
                (target (field "selectionStart" Int)))
             (target (field "selectionEnd" Int))
         else
           const
             {
               Vdom.msg = None;
               prevent_default = false;
               stop_propagation = false;
             })
       (app
          (app (const (fun k s -> (k, s))) (field "key" String))
          (field "shiftKey" Bool)))

(* Clicking in the editor moves the caret; the error whose span it lands in
   becomes the active one. Where the caret ends up is read off the event, the
   model not tracking it. *)
let oncaret_at =
  let open Vdom.Decoder in
  on "click"
    (map
       (fun offset -> Some (Model.CaretAt offset))
       (field "target" (field "selectionStart" Int)))

(* The editor proper: the highlighted text, with the errors' spans marked, and
   the transparent textarea stretched over it. *)
let view_editor ~marks (model : Model.edit_model) =
  let lines = String.split_on_char '\n' model.unparsed_code |> List.length in
  let rows = max 10 lines in
  let highlighted =
    SyntaxHighlight.highlight_with_marks ~line_numbers:true ~marks
      (model.unparsed_code ^ "\n")
  in
  (* The gutter is a small inset, the line numbers (0.558rem a digit at the
     editor's 0.9rem) and a gap; in rem so the marker's smaller font can use it. *)
  let gutter =
    Printf.sprintf "calc(0.36rem + %d * 0.558rem + 0.63rem)"
      (String.length (string_of_int lines))
  in
  div
    ~a:[ class_ "code-editor"; style "--gutter" gutter ]
    [
      elt "pre" ~a:[ class_ "code-editor-display syn-ml" ] highlighted;
      elt "textarea"
        ~a:
          [
            class_ "code-editor-input";
            (* bound as a property, so that loading an example replaces
               what the user has typed; a text child would only set the
               default value, which the browser ignores once the textarea
               has been edited *)
            str_prop "value" model.unparsed_code;
            oninput (fun input -> Model.EditMsg (Model.ChangeSource input));
            oninsert_indent;
            oncaret_at;
            int_prop "rows" rows;
            attr "placeholder"
              "Type a program, or load an example from the right";
            attr "spellcheck" "false";
            attr "autocapitalize" "off";
            attr "autocorrect" "off";
          ]
        [];
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
        (* every error shown under the editor, which may be far below when the
           program is long; clicking one scrolls to its message *)
        (match model.run_model with
        | Error (_ :: _ as errors) ->
            let view_entry i error =
              elt "li"
                ~a:
                  ((if model.active_error = Some i then [ class_ "is-active" ]
                    else [])
                  @ [
                      onmouseenter (fun _ -> Model.HoverError (Some i));
                      onmouseleave (fun _ -> Model.HoverError None);
                    ])
                [
                  elt "a"
                    ~a:
                      [
                        attr "href"
                          ("#"
                          ^
                          if model.stale_errors then error_id i
                          else load_error_target i error);
                      ]
                    [ text (load_error_summary error) ];
                ]
            in
            elt "ol"
              ~a:
                [
                  class_
                    (if model.stale_errors then "error-list is-stale"
                     else "error-list");
                ]
              (List.mapi view_entry errors)
        | _ -> nil);
      ]
  in
  panel "Code options"
    [ use_stdlib; load_example; select_resource; run_process ]

let edit_view (model : Model.model) =
  let errors =
    match model.run_model with Error errors -> errors | Ok _ -> []
  in
  let stale = model.stale_errors in
  (* Edited source: the spans have moved, so only the messages remain. *)
  let marks =
    if stale then []
    else
      List.concat
        (List.mapi
           (fun i error ->
             marks_of_error ~hovered:(model.hovered_error = Some i) i error)
           errors)
  in
  view_contents
    [
      div
        ~a:[ class_ "box editor-box" ]
        (view_editor ~marks model.edit_model
        :: List.mapi
             (fun i error ->
               view_load_error ~stale
                 ~active:(model.active_error = Some i)
                 i error)
             errors);
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
              class_ "button is-outlined is-fullwidth is-danger";
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
              class_ "button is-outlined is-fullwidth";
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
              ~a:[ class_ "control" ]
              [
                select
                  ~a:[ class_ "select is-info" ]
                  "Step size"
                  (fun step_size ->
                    Model.RunMsg (Model.ChangeRandomStepSize step_size))
                  string_of_int
                  (fun step_size -> step_size = run_model.random_step_size)
                  [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512; 1024 ];
              ];
            div
              ~a:[ class_ "control is-expanded" ]
              [
                elt "button"
                  ~a:
                    [
                      class_ "button is-info is-fullwidth";
                      onclick (fun _ -> Model.RunMsg Model.RandomStep);
                      disabled (steps = []);
                    ]
                  [ text "next steps" ];
              ];
          ];
        (if steps = [] then
           elt "p"
             ~a:[ class_ "terminated-note" ]
             [ text "Computation has terminated" ]
         else text "");
      ]
  in
  panel "Interaction"
    ~a:[ onmouseleave (fun _ -> Model.RunMsg (Model.SelectStepIndex None)) ]
    (view_edit_source :: view_undo_last_step :: view_random_steps steps
   :: List.mapi view_step steps)

let run_view (run_model : Model.run_model) =
  let steps = run_model.current.steps in
  (* The index outlives the step list it points into: taking the last step
     empties the list while the pointer is still on the button. *)
  let selected_step =
    Option.bind run_model.selected_step_index (List.nth_opt steps)
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
        div
          ~a:[ class_ "navbar-item brand-title" ]
          [
            elt "img"
              ~a:
                [
                  class_ "brand-logo";
                  attr "src" "logo/temporal-millet-logo.svg";
                  attr "alt" "";
                  attr "width" "56";
                  attr "height" "56";
                ]
              [];
            div
              ~a:[ class_ "brand-text" ]
              [
                elt "p" ~a:[ class_ "title" ] [ text "Temporal Millet" ];
                elt "p"
                  ~a:[ class_ "brand-tagline" ]
                  [
                    text
                      "A language for programming with temporal resources, \
                       using modal types and graded effects";
                  ];
              ];
          ];
      ]
  in

  elt "navbar" ~a:[ class_ "navbar" ] [ view_title ]

let view_footer =
  elt "footer"
    ~a:[ class_ "site-footer" ]
    [
      elt "a"
        ~a:
          [
            class_ "github-link";
            attr "href" github_url;
            attr "target" "_blank";
            attr "rel" "noopener";
            attr "title" "Source code on GitHub";
          ]
        [
          elt "span" ~a:[ class_ "icon" ] [ github_mark ];
          elt "span" [ text "Source on GitHub" ];
        ];
    ]

let view (model : Model.model) =
  div
    [
      view_navbar;
      (match model.run_model with
      | Error _ -> edit_view model
      | Ok run_model -> run_view run_model);
      view_footer;
    ]
