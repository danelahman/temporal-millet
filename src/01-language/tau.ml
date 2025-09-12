module type S = sig
  type t

  val zero : t
  val sequence : t -> t -> t
  val join : t -> t -> t
  val contains : t -> t -> bool
  val compare : t -> t -> int

  val of_int : int -> t
  (** Only necessary because the grammar currently enforces temporal values to
      be natural numbers. **)

  val show : t -> string
end

module NatTau : S = struct
  type t = int

  let zero = 0
  let sequence = ( + )

  let join i j =
    if i = j then i
    else failwith "Temporal grades of program branches are not equal"

  let contains = ( <= )
  let compare = compare

  let of_int i =
    if i < 0 then failwith "NatTau.of_int: expected non-negative integer" else i

  let show = string_of_int
end

module TauImpl = NatTau
