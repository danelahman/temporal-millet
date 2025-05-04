module type S = sig
  type t

  val zero : t
  val add : t -> t -> t
  val subtract : t -> t -> t
  val greater : t -> t -> bool

  val of_int : int -> t
  (** Only necessary because the grammar currently enforces temporal values to
      be natural numbers. **)

  val show : t -> string
end

module NatTau : S = struct
  type t = int

  let zero = 0
  let add = ( + )

  let subtract x y =
    let result = x - y in
    if result < 0 then invalid_arg "NatTau.subtract: result would be negative"
    else result

  let greater = ( > )

  let of_int i =
    if i < 0 then invalid_arg "NatTau.of_int: expected non-negative integer"
    else i

  let show = string_of_int
end

module TauImpl = NatTau
