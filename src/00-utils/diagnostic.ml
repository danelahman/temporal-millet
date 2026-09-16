type kind = Syntax | Typing | Runtime | Fatal
type label = { span : Location.t; text : string }

type t = {
  kind : kind;
  primary : Location.t option;
  message : string;
  labels : label list;
  notes : string list;
}

let kind_to_string = function
  | Syntax -> "Syntax error"
  | Typing -> "Typing error"
  | Runtime -> "Runtime error"
  | Fatal -> "Fatal error"

let lines text = String.split_on_char '\n' text

(* The span's first line with carets under it, from the start of the span to
   its end or to the end of the line, whichever comes first. A multi-line span
   is marked on its first line only, as the compiler does it. *)
let print_excerpt ~source (loc : Location.t) ppf =
  match source loc.filename with
  | None -> ()
  | Some text -> (
      match List.nth_opt (lines text) (loc.start.line - 1) with
      | None -> ()
      | Some line ->
          let width = String.length line in
          let from = min (loc.start.column - 1) width in
          let until =
            if loc.stop.line = loc.start.line then
              min (loc.stop.column - 1) width
            else width
          in
          let carets = max 1 (until - from) in
          let number = string_of_int loc.start.line in
          Format.fprintf ppf "%s | %s@\n%s   %s%s@\n" number line
            (String.make (String.length number) ' ')
            (String.make from ' ') (String.make carets '^'))

let print_located ~source ~indent loc text ppf =
  (* Location, excerpt and text are indented by [indent] and by nothing else,
     so that the carets still line up with the source line above them. *)
  Format.fprintf ppf "%s%t:@\n" indent (Location.print loc);
  Format.fprintf ppf "%t" (fun ppf ->
      let buffer = Buffer.create 64 in
      let inner = Format.formatter_of_buffer buffer in
      print_excerpt ~source loc inner;
      Format.pp_print_flush inner ();
      List.iter
        (fun line ->
          if line <> "" then Format.fprintf ppf "%s%s@\n" indent line)
        (lines (Buffer.contents buffer)));
  match text with "" -> () | text -> Format.fprintf ppf "%s%s@\n" indent text

let print ?(source = fun _ -> None) d ppf =
  (match d.primary with
  | Some loc ->
      Format.fprintf ppf "%t:@\n" (Location.print loc);
      print_excerpt ~source loc ppf
  | None -> ());
  Format.fprintf ppf "%s: %s@\n" (kind_to_string d.kind) d.message;
  List.iter
    (fun { span; text } -> print_located ~source ~indent:"  " span text ppf)
    d.labels;
  List.iter (fun note -> Format.fprintf ppf "  Note: %s@\n" note) d.notes
