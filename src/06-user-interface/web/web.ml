(* Scrolling to an element is a side effect on the page, so it is a command:
   the handler runs it once the view has been redrawn, when the element
   exists. Only errors ask for it, see [update]. *)
type 'msg Vdom.Cmd.t += Scroll_to of string  (** the id of an element *)

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

let scroll_handler =
  {
    Vdom_blit.Cmd.f =
      (fun ctx cmd ->
        match cmd with
        | Scroll_to id ->
            Vdom_blit.Cmd.after_redraw ctx (fun () -> scroll_to id);
            true
        | _ -> false);
  }

let update model msg =
  let model' = Model.update model msg in
  let cmd =
    match (msg, model'.Model.run_model) with
    | Model.RunCode, Error (Some error) ->
        Scroll_to (View.load_error_target error)
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
