(* Lightweight pre-parse helpers for source text. Used to extract bits of
   metadata (currently just the [resources <name>] declaration) before
   committing to a particular resource grade in the parser, which otherwise
   raises on unsupported literal forms. *)

let is_ident_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '_' || c = '\''

(* Skip whitespace and (nested) OCaml-style comments starting at [i].
   Returns the new index. *)
let rec skip_trivia s n i =
  if i >= n then i
  else
    let c = s.[i] in
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' then skip_trivia s n (i + 1)
    else if i + 1 < n && c = '(' && s.[i + 1] = '*' then
      skip_trivia s n (skip_comment s n (i + 2) 1)
    else i

and skip_comment s n i depth =
  if depth = 0 || i >= n then i
  else if i + 1 < n && s.[i] = '(' && s.[i + 1] = '*' then
    skip_comment s n (i + 2) (depth + 1)
  else if i + 1 < n && s.[i] = '*' && s.[i + 1] = ')' then
    skip_comment s n (i + 2) (depth - 1)
  else skip_comment s n (i + 1) depth

let read_ident s n i =
  let start = i in
  let j = ref i in
  while !j < n && is_ident_char s.[!j] do
    incr j
  done;
  if !j > start then Some (String.sub s start (!j - start), !j) else None

(* Find a top-level [resources <name>] declaration anywhere in [source].
   Returns the name (with the trailing [- <name>] segment, if present) of the
   first such declaration found, or [None] if there isn't one. The scan is
   intentionally permissive: it only looks at lexical position, ignoring
   strings, so a stray "resources" inside a string literal would confuse it.
   In practice the keyword is reserved and not used elsewhere. *)
let find_resources_declaration source =
  let n = String.length source in
  let i = ref 0 in
  let found = ref None in
  while !found = None && !i < n do
    i := skip_trivia source n !i;
    match read_ident source n !i with
    | None -> incr i
    | Some (tok, j) ->
        i := j;
        if tok = "resources" then begin
          let k = skip_trivia source n !i in
          match read_ident source n k with
          | None -> ()
          | Some (name1, k1) ->
              let rec extend name pos =
                let k2 = skip_trivia source n pos in
                if k2 < n && source.[k2] = '-' then
                  let k3 = skip_trivia source n (k2 + 1) in
                  match read_ident source n k3 with
                  | Some (segment, k4) -> extend (name ^ "-" ^ segment) k4
                  | None -> name
                else name
              in
              found := Some (extend name1 k1)
        end
  done;
  !found
