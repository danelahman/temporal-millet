module type S = sig
  type t

  val compare : t -> t -> int
  val fresh : string -> t
  val refresh : t -> t
  val print : t -> Format.formatter -> unit
  val string_of : t -> string
end

module Make () : S = struct
  type t = int * string

  let compare (n1, _) (n2, _) = Int.compare n1 n2
  let count = ref (-1)

  let fresh ann =
    incr count;
    (!count, ann)

  let refresh (_, ann) = fresh ann
  let print (_n, ann) ppf = Format.fprintf ppf "%s" ann
  let string_of (_, ann) = ann
end
