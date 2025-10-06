module type S = sig
  type t

  val zero : t
  val add : t -> t -> t
  val greater : t -> t -> bool
  val meet : t -> t -> t

  val of_int : int -> t
  (** Only necessary because the grammar currently enforces temporal values to
      be natural numbers. **)

  val show : t -> string
end

module NatTau : S = struct
  type t = int

  let zero = 0
  let add = ( + )
  let greater = ( > )
  let meet = min

  let of_int i =
    if i < 0 then invalid_arg "NatTau.of_int: expected non-negative integer"
    else i

  let show = string_of_int
end

module TauImpl = NatTau
