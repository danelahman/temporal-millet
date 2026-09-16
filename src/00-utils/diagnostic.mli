(** Diagnostics: what is reported when a program is rejected — a headline, the
    place it points at, labelled related places and free-form notes. The text is
    rendered when the diagnostic is made, since it mentions types and grades the
    user interfaces do not know; the locations stay structured, since the
    interfaces act on them. *)

type kind = Syntax | Typing | Runtime | Fatal

type label = { span : Location.t; text : string }
(** A related place, with what it contributes to the error: where a variable was
    bound, where time passed, where a type was annotated. *)

type t = {
  kind : kind;
  primary : Location.t option;  (** the place the diagnostic points at *)
  message : string;  (** one sentence, not ending in a full stop *)
  labels : label list;
  notes : string list;  (** the constraint that failed, a witness, ... *)
}

val kind_to_string : kind -> string
(** ["Syntax error"], ["Typing error"], ... *)

val print : ?source:(string -> string option) -> t -> Format.formatter -> unit
(** [print ~source d] prints [d] for a terminal: the primary location in the
    compiler's format, so that editors can jump to it, then the headline, the
    labels with their locations, and the notes. Where [source] yields a file's
    text, the source line is shown with the span marked. *)
