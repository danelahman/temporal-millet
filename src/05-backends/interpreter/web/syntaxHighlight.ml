(* Tokenizer that turns a string of pretty-printed ML-style code into a list
   of Vdom nodes with class-tagged spans for syntax highlighting.

   Classification happens once, as offsets into the text ([tokens]); rendering
   is separate ([highlight_text], [highlight_with_marks]). That way the editor
   can overlay error spans without first cutting the text into independently
   tokenized pieces, which would break a comment or string that an error
   starts inside. *)

let keywords =
  [
    "let";
    "in";
    "fun";
    "function";
    "rec";
    "match";
    "with";
    "as";
    "if";
    "then";
    "else";
    "true";
    "false";
    "of";
    "type";
    "noneternal";
    "operation";
    "default";
    "run";
    "return";
    "perform";
    "handle";
    "handler";
    "continue";
    "box";
    "unbox";
    "delay";
    "and";
    "within";
  ]

let is_lower c = c >= 'a' && c <= 'z'
let is_upper c = c >= 'A' && c <= 'Z'
let is_digit c = c >= '0' && c <= '9'
let is_ident_start c = is_lower c || is_upper c || c = '_'
let is_ident_char c = is_ident_start c || is_digit c || c = '\''

(* Greek letters used by the type printer occupy two bytes in UTF-8 (0xCE/0xCF
   lead byte).  We treat the byte sequence as a single ident-like token so the
   highlighter does not split it. *)
let is_greek_lead b = b = 0xCE || b = 0xCF

(* Byte used by the state printer to bracket resource names that appear as
   binding labels (and only those — references to resources inside stored
   values are left unmarked). The byte must not occur in any user-visible
   string; 0x01 is safe in pretty-printer output. *)
let resource_label_marker = '\x01'

(* Byte used by the state printer to bracket the entire entry of the resource
   currently being acted on by the redex (e.g. the resource being unboxed),
   so the web interface can highlight it the same way as the active redex. *)
let active_state_marker = '\x02'

type token = { start : int; stop : int; cls : string option }
(** A maximal run of one highlighting class, or none. Tokens tile the text:
    consecutive, non-overlapping, covering every byte. *)

(** [tokens s] classifies [s] into the runs [highlight_text] colours. *)
let tokens s =
  let n = String.length s in
  (* built in reverse, adjacent runs of the same class coalesced *)
  let toks = ref [] in
  let emit start stop cls =
    if stop > start then
      match !toks with
      | { start = start'; stop = stop'; cls = cls' } :: toks'
        when stop' = start && cls' = cls ->
          toks := { start = start'; stop; cls } :: toks'
      | _ -> toks := { start; stop; cls } :: !toks
  in
  (* Lightweight context used to distinguish operation names (declared with
     [operation], called with [perform], or matched in a [handler] clause)
     from data constructors. Both look like uppercase identifiers; only the
     surrounding tokens tell them apart. *)
  let after_op_kw = ref false in
  let after_bar = ref false in
  let last_block = ref `None in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    let start = !i in
    if
      c = '('
      && !i + 1 < n
      && s.[!i + 1] = '*'
      (* Don't treat "(*)" as a comment opener — it's the multiplication
         operator used as a value, the same convention as OCaml's lexer. *)
      && (!i + 2 >= n || s.[!i + 2] <> ')')
    then begin
      (* OCaml-style nested comment. Comments are trivia and do not reset
         the surrounding-token context. *)
      i := !i + 2;
      let depth = ref 1 in
      while !depth > 0 && !i < n do
        if !i + 1 < n && s.[!i] = '(' && s.[!i + 1] = '*' then begin
          incr depth;
          i := !i + 2
        end
        else if !i + 1 < n && s.[!i] = '*' && s.[!i + 1] = ')' then begin
          decr depth;
          i := !i + 2
        end
        else incr i
      done;
      emit start !i (Some "syn-comment")
    end
    else if c = '"' then begin
      incr i;
      while !i < n && s.[!i] <> '"' do
        if s.[!i] = '\\' && !i + 1 < n then i := !i + 2 else incr i
      done;
      if !i < n then incr i;
      emit start !i (Some "syn-str");
      after_op_kw := false;
      after_bar := false
    end
    else if is_digit c then begin
      while !i < n && (is_digit s.[!i] || s.[!i] = '.') do
        incr i
      done;
      emit start !i (Some "syn-num");
      after_op_kw := false;
      after_bar := false
    end
    else if c = resource_label_marker then begin
      (* The token covers the two markers as well, so that the tokens keep
         tiling the text; the renderer drops the marker bytes. *)
      incr i;
      while !i < n && s.[!i] <> resource_label_marker do
        incr i
      done;
      if !i < n then incr i;
      emit start !i (Some "syn-resource")
    end
    else if c = '|' && (!i + 1 >= n || s.[!i + 1] <> '|') then begin
      (* A lone [|] starts a pattern clause. [||] is logical-or and does
         not. *)
      incr i;
      emit start !i None;
      after_bar := true;
      after_op_kw := false
    end
    else if is_ident_start c then begin
      while !i < n && is_ident_char s.[!i] do
        incr i
      done;
      let tok = String.sub s start (!i - start) in
      if List.mem tok keywords then begin
        emit start !i (Some "syn-kw");
        (match tok with
        | "operation" | "perform" -> after_op_kw := true
        | _ -> after_op_kw := false);
        (match tok with
        | "handler" -> last_block := `Handler
        | "match" | "function" -> last_block := `Match
        | _ -> ());
        after_bar := false
      end
      else if is_upper c then begin
        let cls =
          if !after_op_kw then "syn-op"
          else if !after_bar && !last_block = `Handler then "syn-op"
          else "syn-ctor"
        in
        emit start !i (Some cls);
        after_op_kw := false;
        after_bar := false
      end
      else begin
        emit start !i None;
        after_op_kw := false;
        after_bar := false
      end
    end
    else if is_greek_lead (Char.code c) && !i + 1 < n then begin
      (* Single Greek letter (two-byte UTF-8); pass through as identifier. *)
      i := !i + 2;
      emit start !i None;
      after_op_kw := false;
      after_bar := false
    end
    else begin
      let was_space = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
      incr i;
      emit start !i None;
      if not was_space then begin
        after_op_kw := false;
        after_bar := false
      end
    end
  done;
  List.rev !toks

type marker = {
  href : string;  (** where it links to, such as the message explaining it *)
  title : string;  (** the headline shown on hovering it *)
}
(** What the line numbers of the lines a mark covers link to. *)

type mark = {
  from : int;
  until : int;
  mark_cls : string;
  id : string option;
  marker : marker option;
}
(** A range of the text to wrap in a class of its own, such as the span of an
    error, with an optional element id to scroll to or link to. Unlike tokens,
    marks may nest, overlap and be given in any order. *)

(* The markers the state printer brackets resource names with are consumed by
   the tokenizer and must not reach the page. *)
let displayed_text s start stop =
  let text = String.sub s start (stop - start) in
  (* [String.contains] raises to say no, and a raise from the depth the editor
     reaches costs more than the scan; [index_opt] returns instead. *)
  match String.index_opt text resource_label_marker with
  | None -> text
  | Some _ -> String.concat "" (String.split_on_char resource_label_marker text)

let node ?id classes text =
  match (classes, id) with
  | [], None -> Vdom.text text
  | _ ->
      let a =
        match classes with
        | [] -> []
        | _ -> [ Vdom.class_ (String.concat " " classes) ]
      in
      let a = match id with None -> a | Some id -> Vdom.attr "id" id :: a in
      Vdom.elt "span" ~a [ Vdom.text text ]

(* Out of the flow, so that the overlay goes on measuring as the textarea laid
   over it does; positioned against the <pre>, so that it lands in the gutter.
   See the [.line-number] rules in web/index.html. *)
let line_number_node ?link n =
  let number =
    match link with
    | None ->
        Vdom.elt "span"
          ~a:[ Vdom.class_ "line-number" ]
          [ Vdom.text (string_of_int n) ]
    (* A line an error covers: the number itself is the link to the message,
       which is why it must stay clickable through the textarea. The ends of
       the span are marked so that the gutter bar can be inset there. *)
    | Some ({ href; title }, first, last) ->
        let classes =
          "line-number is-error"
          ^ (if first then " is-error-start" else "")
          ^ if last then " is-error-end" else ""
        in
        Vdom.elt "a"
          ~a:
            [
              Vdom.class_ classes;
              Vdom.attr "href" href;
              Vdom.attr "title" title;
            ]
          [ Vdom.text (string_of_int n) ]
  in
  Vdom.elt "span" ~a:[ Vdom.class_ "line-number-anchor" ] [ number ]

let highlight_text s =
  List.map
    (fun { start; stop; cls } ->
      node (Option.to_list cls) (displayed_text s start stop))
    (tokens s)

type piece = {
  p_from : int;
  p_until : int;
  p_cls : string;
  p_id : string option;
}
(** A mark clipped to one line: its background must not run over the newline
    ending the line, nor over the indentation a continuation line opens with. *)

(* Ascending runs of cut points, merged rather than sorted: the token stops and
   the line starts already rise, so one pass over them suffices. Tail-recursive,
   the editor's text running to tens of thousands of cuts. *)
let rec merge_cuts acc xs ys =
  match (xs, ys) with
  | [], zs | zs, [] -> List.rev_append acc zs
  | x :: xs', y :: ys' ->
      if x < y then merge_cuts (x :: acc) xs' ys
      else if y < x then merge_cuts (y :: acc) xs ys'
      else merge_cuts (x :: acc) xs' ys'

(* The pieces covering a segment are kept in the order the marks were given,
   that being the order their classes and ids go on it. *)
let rec insert_active i = function
  | j :: rest when j < i -> j :: insert_active i rest
  | active -> i :: active

(** [highlight_with_marks ~marks s] highlights [s] as [highlight_text] does and
    wraps each mark's range in its class. Cutting at every token and mark
    boundary makes each segment lie inside one token and wholly inside or
    outside each mark, so it carries that token's class and every mark's. *)
let highlight_with_marks ?(line_numbers = false) ~marks s =
  let n = String.length s in
  let clamp i = max 0 (min n i) in
  let marks =
    Array.of_list
      (List.filter_map
         (fun mark ->
           let from = clamp mark.from and until = clamp mark.until in
           (* A point span, as a lexer error's location is, would mark nothing;
              widen it to the byte it points at so that it can be seen. *)
           let from, until =
             if from < until then (from, until)
             else if from < n then (from, from + 1)
             else (max 0 (n - 1), n)
           in
           if from < until then Some { mark with from; until } else None)
         marks)
  in
  let count = Array.length marks in
  (* The editor appends a newline the source has not got, so the line it opens
     is not one of the source's and is left unnumbered. *)
  let starts =
    if (not line_numbers) && count = 0 then [||]
    else begin
      let starts = ref [ 0 ] in
      String.iteri
        (fun i c -> if c = '\n' && i + 1 < n then starts := (i + 1) :: !starts)
        s;
      Array.of_list (List.rev !starts)
    end
  in
  let lines = Array.length starts in
  let content_stop k =
    let stop = if k + 1 < lines then starts.(k + 1) else n in
    if stop > starts.(k) && s.[stop - 1] = '\n' then stop - 1 else stop
  in
  let line_of offset =
    let lo = ref 0 and hi = ref (lines - 1) in
    while !lo < !hi do
      let mid = (!lo + !hi + 1) / 2 in
      if starts.(mid) <= offset then lo := mid else hi := mid - 1
    done;
    !lo
  in
  (* Each mark is cut into one piece per line it covers, the continuation lines
     starting at their first non-blank byte and no piece reaching the newline. *)
  let pieces = ref [] in
  Array.iter
    (fun m ->
      let first = line_of m.from in
      let id = ref m.id and any = ref false and k = ref first in
      while !k < lines && starts.(!k) < m.until do
        let stop = min m.until (content_stop !k) in
        let start = ref (if !k = first then m.from else starts.(!k)) in
        if !k <> first then
          while !start < stop && (s.[!start] = ' ' || s.[!start] = '\t') do
            incr start
          done;
        if !start < stop then begin
          any := true;
          pieces :=
            { p_from = !start; p_until = stop; p_cls = m.mark_cls; p_id = !id }
            :: !pieces;
          id := None
        end;
        incr k
      done;
      (* A span landing wholly on a newline would leave nothing to carry its
         id; keep it whole rather than lose the link. *)
      if not !any then
        pieces :=
          {
            p_from = m.from;
            p_until = m.until;
            p_cls = m.mark_cls;
            p_id = m.id;
          }
          :: !pieces)
    marks;
  let pieces = Array.of_list (List.rev !pieces) in
  let total = Array.length pieces in
  (* Which error each line's number links to, and whether the line opens or
     closes its span; the first mark given wins, so later ones go down first. *)
  let linked = if line_numbers then Array.make (max lines 1) None else [||] in
  if line_numbers then
    for i = count - 1 downto 0 do
      match marks.(i).marker with
      | None -> ()
      | Some m ->
          let first = line_of marks.(i).from
          and last = line_of (marks.(i).until - 1) in
          for k = first to last do
            linked.(k) <- Some (m, k = first, k = last)
          done
    done;
  let toks = ref (tokens s) in
  (* The tokens tile the text, so their stops are all their bounds but 0. *)
  let cuts =
    merge_cuts []
      (merge_cuts []
         (0 :: List.map (fun { stop; _ } -> stop) !toks)
         (if line_numbers then Array.to_list starts else []))
      (List.sort_uniq compare
         (Array.fold_left (fun acc p -> p.p_from :: p.p_until :: acc) [] pieces))
  in
  (* A piece joins the active set at its start and leaves at its end, so it is
     looked at twice rather than once per segment. *)
  let by_start = Array.init total (fun i -> i) in
  Array.sort (fun a b -> compare pieces.(a).p_from pieces.(b).p_from) by_start;
  let placed = Array.make (max total 1) false in
  let next = ref 0 and active = ref [] and line = ref 0 in
  let out = ref [] in
  let push node = out := node :: !out in
  let rec sweep = function
    | start :: (stop :: _ as rest) ->
        (* [!toks] is sorted, so the token a segment lies in is found by
           dropping the tokens that end before the segment starts. *)
        while
          match !toks with { stop; _ } :: _ -> stop <= start | [] -> false
        do
          toks := List.tl !toks
        done;
        if line_numbers && !line < lines && starts.(!line) = start then begin
          push (line_number_node ?link:linked.(!line) (!line + 1));
          incr line
        end;
        while !next < total && pieces.(by_start.(!next)).p_from <= start do
          let i = by_start.(!next) in
          if pieces.(i).p_until > start then active := insert_active i !active;
          incr next
        done;
        active := List.filter (fun i -> pieces.(i).p_until > start) !active;
        let covering = !active in
        let cls =
          match !toks with { cls; _ } :: _ -> Option.to_list cls | [] -> []
        in
        let classes = cls @ List.map (fun i -> pieces.(i).p_cls) covering in
        (* A mark's id goes on the first segment it covers; where two marks
           start together the extra ids get empty spans, an element having but
           one id. *)
        let ids =
          List.filter_map
            (fun i ->
              match pieces.(i).p_id with
              | Some id when not placed.(i) ->
                  placed.(i) <- true;
                  Some id
              | _ -> None)
            covering
        in
        let id =
          match ids with
          | [] -> None
          | id :: extra ->
              List.iter (fun id -> push (node ~id [] "")) extra;
              Some id
        in
        push (node ?id classes (displayed_text s start stop));
        sweep rest
    | _ -> ()
  in
  sweep cuts;
  List.rev !out
