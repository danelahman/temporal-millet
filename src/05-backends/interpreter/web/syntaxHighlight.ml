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

type mark = { from : int; until : int; mark_cls : string; id : string option }
(** A range of the text to wrap in a class of its own, such as the span of an
    error, with an optional element id to scroll to or link to. Unlike tokens,
    marks may nest, overlap and be given in any order. *)

(* The markers the state printer brackets resource names with are consumed by
   the tokenizer and must not reach the page. *)
let displayed_text s start stop =
  let text = String.sub s start (stop - start) in
  if String.contains text resource_label_marker then
    String.concat "" (String.split_on_char resource_label_marker text)
  else text

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

let highlight_text s =
  List.map
    (fun { start; stop; cls } ->
      node (Option.to_list cls) (displayed_text s start stop))
    (tokens s)

(** [highlight_with_marks ~marks s] highlights [s] as [highlight_text] does and
    wraps each mark's range in its class. Cutting at every token and mark
    boundary makes each segment lie inside one token and wholly inside or
    outside each mark, so it carries that token's class and every mark's. *)
let highlight_with_marks ~marks s =
  let n = String.length s in
  let clamp i = max 0 (min n i) in
  let marks =
    List.filter_map
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
      marks
  in
  let toks = tokens s in
  let cuts =
    List.sort_uniq compare
      (0 :: n
       :: List.concat_map (fun { start; stop; _ } -> [ start; stop ]) toks
      @ List.concat_map (fun { from; until; _ } -> [ from; until ]) marks)
  in
  (* [toks] and [cuts] are both sorted, so the token a segment lies in is
     found by dropping the tokens that end before the segment starts. *)
  let rec drop_before start = function
    | { stop; _ } :: toks when stop <= start -> drop_before start toks
    | toks -> toks
  in
  let rec segments placed toks = function
    | start :: (stop :: _ as cuts) ->
        let toks = drop_before start toks in
        let cls =
          match toks with { cls; _ } :: _ -> Option.to_list cls | [] -> []
        in
        let covering =
          List.filter (fun m -> m.from <= start && stop <= m.until) marks
        in
        (* A mark's id goes on the first segment it covers; where two marks
           start together the extra ids get empty spans, an element having but
           one id. *)
        let ids =
          List.filter_map
            (fun m ->
              match m.id with
              | Some id when not (List.mem id placed) -> Some id
              | _ -> None)
            covering
        in
        let classes = cls @ List.map (fun m -> m.mark_cls) covering in
        let anchors, id =
          match ids with
          | [] -> ([], None)
          | id :: extra -> (List.map (fun id -> node ~id [] "") extra, Some id)
        in
        (anchors @ [ node ?id classes (displayed_text s start stop) ])
        @ segments (ids @ placed) toks cuts
    | _ -> []
  in
  segments [] toks cuts
