(** Finite maps keyed by plain strings, used for the tables the compiler keeps
    under the surface names of the program. *)

include Map.Make (String)
