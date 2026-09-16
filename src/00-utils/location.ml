type pos = { line : int; column : int; offset : int }
type t = { filename : string; start : pos; stop : pos }
type 'a located = { it : 'a; at : t }

let pos_of_lexing (p : Lexing.position) =
  {
    line = p.pos_lnum;
    column = p.pos_cnum - p.pos_bol + 1;
    offset = p.pos_cnum;
  }

let of_lexing (start : Lexing.position) (stop : Lexing.position) =
  {
    filename = start.pos_fname;
    start = pos_of_lexing start;
    stop = pos_of_lexing stop;
  }

let of_lexbuf lexbuf =
  of_lexing (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)

let compare_pos p1 p2 = Int.compare p1.offset p2.offset

let merge l1 l2 =
  {
    filename = l1.filename;
    start = (if compare_pos l1.start l2.start <= 0 then l1.start else l2.start);
    stop = (if compare_pos l1.stop l2.stop >= 0 then l1.stop else l2.stop);
  }

let compare l1 l2 =
  match String.compare l1.filename l2.filename with
  | 0 -> (
      match compare_pos l1.start l2.start with
      | 0 -> compare_pos l2.stop l1.stop
      | c -> c)
  | c -> c

let equal l1 l2 = compare l1 l2 = 0
let is_point l = l.start.offset = l.stop.offset

(* The compiler's convention: characters are columns counted from 0, the end
   one exclusive and, for a multi-line span, a column of its last line. *)
let print_lines_and_chars { start; stop; _ } ppf =
  if start.line = stop.line then
    Format.fprintf ppf "line %d, characters %d-%d" start.line (start.column - 1)
      (stop.column - 1)
  else
    Format.fprintf ppf "lines %d-%d, characters %d-%d" start.line stop.line
      (start.column - 1) (stop.column - 1)

let print_short = print_lines_and_chars

let print loc ppf =
  if loc.filename <> "" then
    Format.fprintf ppf "File %S, %t" loc.filename (print_lines_and_chars loc)
  else print_lines_and_chars loc ppf
