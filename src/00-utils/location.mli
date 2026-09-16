(** Source code locations: spans of one source file, whose positions carry a
    line and a column for people and an offset for programs, such as an editor
    slicing the text it shows. *)

type pos = { line : int; column : int; offset : int }
(** Line and column count from 1, the offset into the whole source from 0. *)

type t = { filename : string; start : pos; stop : pos }
(** A span of the file [filename], from [start] inclusive to [stop] exclusive. A
    point location has [start = stop]. *)

type 'a located = { it : 'a; at : t }

val of_lexing : Lexing.position -> Lexing.position -> t
(** [of_lexing start stop] is the span between two positions of one file. *)

val of_lexbuf : Lexing.lexbuf -> t
(** [of_lexbuf lexbuf] is the span of the lexeme the lexer has just read. *)

val merge : t -> t -> t
(** [merge l1 l2] is the smallest span covering both [l1] and [l2], which must
    be spans of the same file. *)

val compare : t -> t -> int
(** Spans are ordered by file name, then by start, then by end, so that a span
    sorts before the spans it contains. *)

val equal : t -> t -> bool

val is_point : t -> bool
(** Whether the span is empty, as the locations of lexer errors are. *)

val print : t -> Format.formatter -> unit
(** Prints the span in the format the OCaml compiler uses, which editors already
    know how to read: [File "f.mlt", line 3, characters 4-9], or [lines 3-5] for
    a span crossing lines. Without a file name the file part is left out. *)

val print_short : t -> Format.formatter -> unit
(** [print_short] is [print] without the file name: [line 3, characters 4-9]. *)
