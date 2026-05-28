(** Concrete representation of grades as they appear in source code. *)
type lit =
  | Int of int  (** A single non-negative integer, e.g. [42] *)
  | Pair of int * int
      (** A pair of integers, e.g. [(1, 5)] for interval grades *)

(** Grades are ordered monoids with optional properties about zero. *)
module type S = sig
  type t

  val name : string
  (** The name of the grade type, e.g. ["time-interval"]. *)

  val zero : t
  val add : t -> t -> t

  val is_sub_rho : t -> t -> bool
  (** Compare whether one grade is a sub-grade of another. *)

  val is_sub_rho_symbol : string
  val is_zero_minimal_sub_rho : bool
  val is_zero_top_sub_rho : bool

  val of_lit : lit -> t
  (** Converts a parsed grade literal to a value of type [t]. *)

  val of_nat : int -> t
  (** Converts a parsed constant into a value of type [t]. *)

  val show : t -> string
end

(* Example definitions of grades. *)

module TimeLowerBoundGrade : S = struct
  type t = int

  let name = "time-lower-bound"
  let zero = 0
  let add = ( + )
  let is_sub_rho = ( >= )
  let is_sub_rho_symbol = ">="
  let is_zero_minimal_sub_rho = false
  let is_zero_top_sub_rho = true

  let of_lit = function
    | Int n ->
        if n < 0 then
          invalid_arg
            "TimeLowerBoundGrade.of_lit: expected non-negative integer"
        else n
    | Pair _ ->
        invalid_arg
          "TimeLowerBoundGrade.of_lit: pair literals are not supported"

  let of_nat n =
    if n < 0 then
      invalid_arg "TimeLowerBoundGrade.of_nat: expected non-negative integer"
    else n

  let show = string_of_int
end

module TimeUpperBoundGrade : S = struct
  type t = int

  let name = "time-upper-bound"
  let zero = 0
  let add = ( + )
  let is_sub_rho = ( <= )
  let is_sub_rho_symbol = "<="
  let is_zero_minimal_sub_rho = true
  let is_zero_top_sub_rho = false

  let of_lit = function
    | Int n ->
        if n < 0 then
          invalid_arg
            "TimeUpperBoundGrade.of_lit: expected non-negative integer"
        else n
    | Pair _ ->
        invalid_arg
          "TimeUpperBoundGrade.of_lit: pair literals are not supported"

  let of_nat n =
    if n < 0 then
      invalid_arg "TimeUpperBoundGrade.of_nat: expected non-negative integer"
    else n

  let show = string_of_int
end

module TimeIntervalGrade : S = struct
  type t = int * int

  let name = "time-interval"
  let zero = (0, 0)
  let add (n, m) (k, l) = (n + k, m + l)

  (** sub-interval order, (n, m) is sub-interval of (k, l) *)
  let is_sub_rho (n, m) (k, l) = n >= k && l >= m

  let is_sub_rho_symbol = "<="
  let is_zero_minimal_sub_rho = true
  let is_zero_top_sub_rho = false

  let of_lit = function
    | Int _ -> invalid_arg "TimeIntervalGrade.of_lit: pair literal expected"
    | Pair (n, m) ->
        if n < 0 then
          invalid_arg "TimeIntervalGrade.of_lit: interval endpoint negative"
        else if n > m then
          invalid_arg "TimeIntervalGrade.of_lit: non-well-formed interval"
        else (n, m)

  let of_nat n =
    if n < 0 then
      invalid_arg "TimeIntervalGrade.of_nat: expected non-negative integer"
    else (n, n)

  let show (n, m) = "(" ^ string_of_int n ^ "," ^ string_of_int m ^ ")"
end
