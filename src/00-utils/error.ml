(** Error reporting *)

exception Error of Diagnostic.t

(** [error ~loc ~labels ~notes kind fmt] raises an [Error] of [kind]. The
    message goes through [Format.kasprintf] into a fresh buffer rather than the
    shared [Format.str_formatter], which is not multidomain-safe. *)
let error ?loc ?(labels = []) ?(notes = []) kind fmt =
  Format.kasprintf
    (fun message ->
      raise (Error { Diagnostic.kind; primary = loc; message; labels; notes }))
    ("@[" ^^ fmt ^^ "@]")

let fatal ?loc fmt = error ?loc Diagnostic.Fatal fmt
let syntax ~loc fmt = error ~loc Diagnostic.Syntax fmt

let typing ?loc ?labels ?notes fmt =
  error ?loc ?labels ?notes Diagnostic.Typing fmt

let runtime ?loc fmt = error ?loc Diagnostic.Runtime fmt
