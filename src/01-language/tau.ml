(** Concrete representation of tau grades as they appear in source. *)
type lit =
  | Int of int  (** A single non-negative integer, e.g. [42] *)
  | Pair of int * int
      (** A pair of integers, e.g. [(1, 5)] for interval grades *)

module type S = sig
  type t

  val zero : t
  val add : t -> t -> t
  val greater : t -> t -> bool

  val of_lit : lit -> t
  (** Converts a parsed tau literal to a value of type [t]. *)

  val of_nat : int -> t
  (** Converts a parsed integer constant into a value of type [t]. *)

  val show : t -> string
end

module NatTau : S = struct
  type t = int

  let zero = 0
  let add = ( + )
  let greater = ( > )

  let of_lit = function
    | Int n ->
        if n < 0 then invalid_arg "NatTau.of_lit: expected non-negative integer"
        else n
    | Pair _ -> invalid_arg "NatTau.of_lit: pair literals are not supported"

  let of_nat n =
    if n < 0 then invalid_arg "NatTau.of_nat: expected non-negative integer"
    else n

  let show = string_of_int
end
