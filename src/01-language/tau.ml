(** Concrete representation of tau grades as they appear in source. *)
type lit =
  | Int of int  (** A single non-negative integer, e.g. [42] *)
  | Pair of int * int
      (** A pair of integers, e.g. [(1, 5)] for interval grades *)

module type S = sig
  type t

  val name : string
  (** The name of the resource grade type, e.g. ["time"] or ["time-interval"].
  *)

  val zero : t
  val add : t -> t -> t

  val is_sub_tau : t -> t -> bool
  (** Compare whether one tau is sub-tau of another. *)

  val is_sub_tau_symbol : string
  val is_zero_minimal_sub_tau : bool
  val is_zero_top_sub_tau : bool

  val of_lit : lit -> t
  (** Converts a parsed tau literal to a value of type [t]. *)

  val of_nat : int -> t
  (** Converts a parsed integer constant into a value of type [t]. *)

  val show : t -> string
end

module NatTau : S = struct
  type t = int

  let name = "time"
  let zero = 0
  let add = ( + )
  let is_sub_tau = ( >= )
  let is_sub_tau_symbol = ">="
  let is_zero_minimal_sub_tau = false
  let is_zero_top_sub_tau = true

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

module IntervalTau : S = struct
  type t = int * int

  let name = "time-interval"
  let zero = (0, 0)
  let add (n, m) (k, l) = (n + k, m + l)

  (** sub-interval order, (n, m) is sub-interval of (k, l) *)
  let is_sub_tau (n, m) (k, l) = n >= k && l >= m

  let is_sub_tau_symbol = "<="
  let is_zero_minimal_sub_tau = true
  let is_zero_top_sub_tau = false

  let of_lit = function
    | Int _ -> invalid_arg "IntervalTau.of_lit: pair literal expected"
    | Pair (n, m) ->
        if n < 0 then
          invalid_arg "IntervalTau.of_lit: interval endpoint negative"
        else if n > m then
          invalid_arg "IntervalTau.of_lit: non-well-formed interval"
        else (n, m)

  let of_nat n =
    if n < 0 then
      invalid_arg "IntervalTau.of_nat: expected non-negative integer"
    else (n, n)

  let show (n, m) = "(" ^ string_of_int n ^ "," ^ string_of_int m ^ ")"
end
