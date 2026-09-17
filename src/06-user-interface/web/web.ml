(* Scrolling to an element is a side effect on the page, so it is a command:
   the handler runs it once the view has been redrawn, when the element
   exists. Only errors ask for it, see [update]. *)
type 'msg Vdom.Cmd.t +=
  | Scroll_to of string  (** the id of an element *)
  | Set_caret of int  (** where the editor's caret goes after a redraw *)

let scroll_to id =
  match Js_browser.Document.get_element_by_id Js_browser.document id with
  | Some element ->
      ignore
        (Ojs.call
           (Js_browser.Element.t_to_js element)
           "scrollIntoView"
           [|
             Ojs.obj
               [|
                 ("behavior", Ojs.string_to_js "smooth");
                 ("block", Ojs.string_to_js "center");
               |];
           |])
  | None -> ()

(* Setting the editor's value from the model leaves the caret at the end; put
   it back where the edit happened. The position is counted in the browser's
   own UTF-16 code units, as the selection it sets is. *)
let set_caret position =
  match
    Js_browser.Document.query_selector_all Js_browser.document
      ".code-editor-input"
  with
  | [ editor ] ->
      Js_browser.Element.set_selection_start editor position;
      Js_browser.Element.set_selection_end editor position
  | _ -> ()

let scroll_handler =
  {
    Vdom_blit.Cmd.f =
      (fun ctx cmd ->
        match cmd with
        | Scroll_to id ->
            Vdom_blit.Cmd.after_redraw ctx (fun () -> scroll_to id);
            true
        | Set_caret position ->
            Vdom_blit.Cmd.after_redraw ctx (fun () -> set_caret position);
            true
        | _ -> false);
  }

let update model msg =
  let model' = Model.update model msg in
  let cmd =
    match (msg, model'.Model.run_model) with
    | Model.RunCode, Error (error :: _) ->
        Scroll_to (View.load_error_target 0 error)
    | Model.EditMsg (Model.InsertIndent (_, start, _)), _ ->
        Set_caret (start + String.length Model.indentation)
    | _ -> Vdom.Cmd.batch []
  in
  (model', cmd)

let app =
  Vdom.app ~init:(Model.init, Vdom.Cmd.batch []) ~view:View.view ~update ()

let run () =
  Vdom_blit.run ~env:(Vdom_blit.cmd scroll_handler) app
  |> Vdom_blit.dom
  |> Js_browser.Element.append_child
       (match
          Js_browser.Document.get_element_by_id Js_browser.document "container"
        with
       | Some element -> element
       | None -> Js_browser.Document.document_element Js_browser.document)

let () = Js_browser.Window.set_onload Js_browser.window run
