(** Error reporting *)

type t = Location.t option * string * string

let print (loc, error_kind, msg) = Print.error ?loc error_kind "%s" msg

exception Error of t

(** [error ~loc error_kind fmt] raises an [Error] of kind [error_kind] with a
    message [fmt] at a location [loc]. We use [Format.kasprintf] so the message
    is built into a fresh buffer rather than the shared [Format.str_formatter],
    which is not safe under OCaml 5 multidomain code. *)
let error ?loc error_kind fmt =
  Format.kasprintf
    (fun msg -> raise (Error (loc, error_kind, msg)))
    ("@[" ^^ fmt ^^ "@]")

let fatal ?loc fmt = error ?loc "Fatal error" fmt
let syntax ~loc fmt = error ~loc "Syntax error" fmt
let typing ?loc fmt = error ?loc "Typing error" fmt
let runtime ?loc fmt = error ?loc "Runtime error" fmt
