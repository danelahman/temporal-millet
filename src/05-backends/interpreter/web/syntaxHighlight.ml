(* Tokenizer that turns a string of pretty-printed ML-style code into a list
   of Vdom nodes with class-tagged spans for syntax highlighting. *)

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
    "operation";
    "resources";
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

let highlight_text s =
  let n = String.length s in
  let nodes = ref [] in
  let buf = Buffer.create 64 in
  (* Lightweight context used to distinguish operation names (declared with
     [operation], called with [perform], or matched in a [handler] clause)
     from data constructors. Both look like uppercase identifiers; only the
     surrounding tokens tell them apart. *)
  let after_op_kw = ref false in
  let after_bar = ref false in
  let last_block = ref `None in
  let flush_text () =
    if Buffer.length buf > 0 then (
      nodes := Vdom.text (Buffer.contents buf) :: !nodes;
      Buffer.clear buf)
  in
  let push_span cls str =
    flush_text ();
    nodes := Vdom.elt "span" ~a:[ Vdom.class_ cls ] [ Vdom.text str ] :: !nodes
  in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if
      c = '(' && !i + 1 < n && s.[!i + 1] = '*'
      (* Don't treat "(*)" as a comment opener — it's the multiplication
         operator used as a value, the same convention as OCaml's lexer. *)
      && (!i + 2 >= n || s.[!i + 2] <> ')')
    then begin
      (* OCaml-style nested comment. Comments are trivia and do not reset
         the surrounding-token context. *)
      let start = !i in
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
      push_span "syn-comment" (String.sub s start (!i - start))
    end
    else if c = '"' then begin
      let start = !i in
      incr i;
      while !i < n && s.[!i] <> '"' do
        if s.[!i] = '\\' && !i + 1 < n then i := !i + 2 else incr i
      done;
      if !i < n then incr i;
      push_span "syn-str" (String.sub s start (!i - start));
      after_op_kw := false;
      after_bar := false
    end
    else if is_digit c then begin
      let start = !i in
      while !i < n && (is_digit s.[!i] || s.[!i] = '.') do
        incr i
      done;
      push_span "syn-num" (String.sub s start (!i - start));
      after_op_kw := false;
      after_bar := false
    end
    else if c = resource_label_marker then begin
      incr i;
      let start = !i in
      while !i < n && s.[!i] <> resource_label_marker do
        incr i
      done;
      let tok = String.sub s start (!i - start) in
      if !i < n then incr i;
      push_span "syn-resource" tok
    end
    else if c = '|' && (!i + 1 >= n || s.[!i + 1] <> '|') then begin
      (* A lone [|] starts a pattern clause. [||] is logical-or and does
         not. *)
      Buffer.add_char buf '|';
      incr i;
      after_bar := true;
      after_op_kw := false
    end
    else if is_ident_start c then begin
      let start = !i in
      while !i < n && is_ident_char s.[!i] do
        incr i
      done;
      let tok = String.sub s start (!i - start) in
      if List.mem tok keywords then begin
        push_span "syn-kw" tok;
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
        push_span cls tok;
        after_op_kw := false;
        after_bar := false
      end
      else begin
        Buffer.add_string buf tok;
        after_op_kw := false;
        after_bar := false
      end
    end
    else if is_greek_lead (Char.code c) && !i + 1 < n then begin
      (* Single Greek letter (two-byte UTF-8); pass through as identifier. *)
      Buffer.add_substring buf s !i 2;
      i := !i + 2;
      after_op_kw := false;
      after_bar := false
    end
    else begin
      let was_space = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
      Buffer.add_char buf c;
      incr i;
      if not was_space then begin
        after_op_kw := false;
        after_bar := false
      end
    end
  done;
  flush_text ();
  List.rev !nodes
