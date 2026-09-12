(** The events a trace literal is made of, re-exported from {!TimedTrace} so
    that the parser can build literals without a conversion step. *)
type trace_event = TimedTrace.event = Ev of string | Wait of int

(** Concrete representation of resource grades as they appear in source. *)
type lit =
  | Int of int  (** A single non-negative integer, e.g. [42] *)
  | Pair of int * int
      (** A pair of integers, e.g. [(1, 5)] for interval grades *)
  | Traces of trace_event list list
      (** A set of timed traces, e.g. [{Read; 3; Send | Send; Send}] *)
  | TracePair of trace_event list list * trace_event list list
      (** A pair of sets of timed traces, e.g. [({3}, {Send | 5})] *)

module type Grade = sig
  type t

  val name : string
  (** The name of the resource grade type, e.g. ["time-interval"]. *)

  val zero : t
  val add : t -> t -> t

  val is_sub_rho : (string -> int * int) -> t -> t -> bool
  (** [is_sub_rho bounds a b] compares whether [a] is a sub-grade of [b]. The
      [bounds] function gives each operation its declared runtime bounds
      [(lo, hi)]; the time grades ignore it. *)

  val is_sub_rho_symbol : string
  val is_zero_minimal_sub_rho : bool
  val is_zero_top_sub_rho : bool

  val is_commutative : bool
  (** Whether the monoid is commutative; true for the time grades, false for the
      trace grades, whose product is the (non-commutative) language product. *)

  val needs_op_bounds : bool
  (** Whether operation signatures must carry their runtime bounds
      [within (lo, hi)]. *)

  val implied_bounds : (string -> int * int) -> t -> (int * int) option
  (** [implied_bounds bounds rho] is the pair of runtime bounds the grade [rho]
      itself implies: the duration of its fastest run, each event counted at the
      lower end of its [bounds], and the duration of its slowest run, each event
      counted at the upper end. The fastest run is taken over the lower-bound
      component of the grade and the slowest over its upper-bound component,
      which coincide for the one-sided trace grades. The time grades imply
      nothing, since there the grade of an operation already is its runtime
      bound, and return [None]. *)

  val events : t -> string list
  (** The operation names mentioned by a grade; empty for the time grades. *)

  val of_lit : lit -> t
  (** Converts a parsed resource grade literal to a value of type [t]. *)

  val of_nat : int -> t
  (** Converts a parsed integer constant into a value of type [t]. *)

  val show : t -> string
end

(** The literal forms the time grades do not understand. Their messages are
    surfaced to the user as located syntax errors naming the grading monoid in
    use (see [Loader.parse_commands]). *)
let reject_trace_lit which =
  invalid_arg
    ("grades are " ^ which
   ^ ", not sets of timed traces; did you mean to use one of the \
      'timed-traces-lower-bound', 'timed-traces-upper-bound' or \
      'timed-traces-interval' grading monoids?")

module TimeLowerBoundGrade : Grade = struct
  type t = int

  let name = "time-lower-bound"
  let zero = 0
  let add = ( + )
  let is_sub_rho _bounds = ( >= )
  let is_sub_rho_symbol = ">="
  let is_zero_minimal_sub_rho = false
  let is_zero_top_sub_rho = true
  let is_commutative = true
  let needs_op_bounds = false
  let events _ = []
  let implied_bounds _bounds _ = None

  let of_lit = function
    | Int n ->
        if n < 0 then invalid_arg "grades must be non-negative integers" else n
    | Pair _ ->
        invalid_arg
          "grades are plain integers, not pairs; did you mean to use the \
           'time-interval' grading monoid?"
    | Traces _ | TracePair _ -> reject_trace_lit "plain integers"

  let of_nat n =
    if n < 0 then
      invalid_arg "TimeLowerBoundGrade.of_nat: expected non-negative integer"
    else n

  let show = string_of_int
end

module TimeUpperBoundGrade : Grade = struct
  type t = int

  let name = "time-upper-bound"
  let zero = 0
  let add = ( + )
  let is_sub_rho _bounds = ( <= )
  let is_sub_rho_symbol = "<="
  let is_zero_minimal_sub_rho = true
  let is_zero_top_sub_rho = false
  let is_commutative = true
  let needs_op_bounds = false
  let events _ = []
  let implied_bounds _bounds _ = None

  let of_lit = function
    | Int n ->
        if n < 0 then invalid_arg "grades must be non-negative integers" else n
    | Pair _ ->
        invalid_arg
          "grades are plain integers, not pairs; did you mean to use the \
           'time-interval' grading monoid?"
    | Traces _ | TracePair _ -> reject_trace_lit "plain integers"

  let of_nat n =
    if n < 0 then
      invalid_arg "TimeUpperBoundGrade.of_nat: expected non-negative integer"
    else n

  let show = string_of_int
end

module IntervalResourceGrade : Grade = struct
  type t = int * int

  let name = "time-interval"
  let zero = (0, 0)
  let add (n, m) (k, l) = (n + k, m + l)

  (** sub-interval order, (n, m) is sub-interval of (k, l) *)
  let is_sub_rho _bounds (n, m) (k, l) = n >= k && l >= m

  let is_sub_rho_symbol = "<="
  let is_zero_minimal_sub_rho = true
  let is_zero_top_sub_rho = false
  let is_commutative = true
  let needs_op_bounds = false
  let events _ = []
  let implied_bounds _bounds _ = None

  let of_lit = function
    | Int _ ->
        invalid_arg
          "grades are intervals '(n, m)', not plain integers; did you mean to \
           use the 'time-lower-bound' or 'time-upper-bound' grading monoid?"
    | Pair (n, m) ->
        if n < 0 then invalid_arg "interval endpoints must be non-negative"
        else if n > m then invalid_arg "interval endpoints must satisfy n <= m"
        else (n, m)
    | Traces _ | TracePair _ -> reject_trace_lit "intervals '(n, m)'"

  let of_nat n =
    if n < 0 then
      invalid_arg "IntervalResourceGrade.of_nat: expected non-negative integer"
    else (n, n)

  let show (n, m) = "(" ^ string_of_int n ^ "," ^ string_of_int m ^ ")"
end

(* The three grades below are graded by sets of timed traces (see
   {!TimedTrace}); their product is the language product and their values are
   always kept in the canonical normal form that module documents, since the
   typechecker, the context and the pretty-printer compare grades structurally.

   Cost model: an operation declares a pair of runtime bounds [within (lo, hi)],
   and the two orders read *different* endpoints — [lo] feeds the coverage
   (lower-bound) order, [hi] feeds the allowance (upper-bound) order. The
   formalisation uses a single [cost] for both; reading the pair instead is the
   [fast]/[slow] generalisation, and it is sound because each order only ever
   needs its own direction of the bound. *)

let trace_of_lit_pair_msg =
  "grades are a single set of timed traces '{...}' or a plain integer; did you \
   mean to use the 'timed-traces-interval' grading monoid?"

module TimedTracesLowerBoundGrade : Grade = struct
  type t = TimedTrace.traces

  let name = "timed-traces-lower-bound"
  let zero = TimedTrace.of_nat 0
  let add = TimedTrace.product

  (** coverage order lifted to sets: every guarantee listed on the left has an
      easier one listed on the right. An operation's [lo] bound is what its
      occurrence in the run banks. *)
  let is_sub_rho bounds = TimedTrace.lower_bound_le (fun op -> fst (bounds op))

  (** ["<="] denotes the sub-grade preorder, as for the interval grade. *)
  let is_sub_rho_symbol = "<="

  let is_zero_minimal_sub_rho = false

  (* everything is below the unit {ε} by the [nil] rule of the coverage order *)
  let is_zero_top_sub_rho = true
  let is_commutative = false
  let needs_op_bounds = true
  let events = TimedTrace.events

  (** the fastest and the slowest run of the set, the events read at their [lo]
      and at their [hi] bound respectively *)
  let implied_bounds bounds p =
    Some
      ( TimedTrace.min_duration (fun op -> fst (bounds op)) p,
        TimedTrace.max_duration (fun op -> snd (bounds op)) p )

  let of_lit = function
    | Int n ->
        if n < 0 then invalid_arg "grades must be non-negative integers"
        else TimedTrace.of_nat n
    | Traces ts -> TimedTrace.of_list ts
    | Pair _ | TracePair _ -> invalid_arg trace_of_lit_pair_msg

  let of_nat n =
    if n < 0 then
      invalid_arg
        "TimedTracesLowerBoundGrade.of_nat: expected non-negative integer"
    else TimedTrace.of_nat n

  let show = TimedTrace.show
end

module TimedTracesUpperBoundGrade : Grade = struct
  type t = TimedTrace.traces

  let name = "timed-traces-upper-bound"
  let zero = TimedTrace.of_nat 0
  let add = TimedTrace.product

  (** allowance order lifted to sets: every bound listed on the left stays
      within some bound listed on the right. An operation's [hi] bound is what
      it costs to buy with banked time. *)
  let is_sub_rho bounds = TimedTrace.upper_bound_le (fun op -> snd (bounds op))

  (** ["<="] denotes the sub-grade preorder, as for the interval grade. *)
  let is_sub_rho_symbol = "<="

  (* the unit {ε} permits nothing but the empty run, by [≼-ε-inv] *)
  let is_zero_minimal_sub_rho = true
  let is_zero_top_sub_rho = false
  let is_commutative = false
  let needs_op_bounds = true
  let events = TimedTrace.events

  (** the fastest and the slowest run of the set, the events read at their [lo]
      and at their [hi] bound respectively *)
  let implied_bounds bounds p =
    Some
      ( TimedTrace.min_duration (fun op -> fst (bounds op)) p,
        TimedTrace.max_duration (fun op -> snd (bounds op)) p )

  let of_lit = function
    | Int n ->
        if n < 0 then invalid_arg "grades must be non-negative integers"
        else TimedTrace.of_nat n
    | Traces ts -> TimedTrace.of_list ts
    | Pair _ | TracePair _ -> invalid_arg trace_of_lit_pair_msg

  let of_nat n =
    if n < 0 then
      invalid_arg
        "TimedTracesUpperBoundGrade.of_nat: expected non-negative integer"
    else TimedTrace.of_nat n

  let show = TimedTrace.show
end

module TimedTracesIntervalGrade : Grade = struct
  type t = TimedTrace.traces * TimedTrace.traces

  let name = "timed-traces-interval"
  let zero = (TimedTrace.of_nat 0, TimedTrace.of_nat 0)

  let add (lo, hi) (lo', hi') =
    (TimedTrace.product lo lo', TimedTrace.product hi hi')

  (** each endpoint at its own order: the lower bound by coverage against [lo],
      the upper bound by allowance against [hi] *)
  let is_sub_rho bounds (lo, hi) (lo', hi') =
    TimedTrace.lower_bound_le (fun op -> fst (bounds op)) lo lo'
    && TimedTrace.upper_bound_le (fun op -> snd (bounds op)) hi hi'

  (** ["<="] denotes the sub-grade preorder, as for the interval grade. *)
  let is_sub_rho_symbol = "<="

  (* neither, since [P, {ε}] is a sub-grade of [{ε}, {ε}] for every [P] *)
  let is_zero_minimal_sub_rho = false
  let is_zero_top_sub_rho = false
  let is_commutative = false
  let needs_op_bounds = true
  let events (lo, hi) = TimedTrace.events (lo @ hi)

  (** the fastest run of the lower-bound component and the slowest run of the
      upper-bound component, each read at its own end of the bounds *)
  let implied_bounds bounds (lo, hi) =
    Some
      ( TimedTrace.min_duration (fun op -> fst (bounds op)) lo,
        TimedTrace.max_duration (fun op -> snd (bounds op)) hi )

  let of_lit = function
    | Int n ->
        if n < 0 then invalid_arg "grades must be non-negative integers"
        else (TimedTrace.of_nat n, TimedTrace.of_nat n)
    | Pair (n, m) ->
        if n < 0 then invalid_arg "interval endpoints must be non-negative"
        else if n > m then invalid_arg "interval endpoints must satisfy n <= m"
        else (TimedTrace.of_nat n, TimedTrace.of_nat m)
    | Traces ts ->
        let ts' = TimedTrace.of_list ts in
        (ts', ts')
    | TracePair (ts1, ts2) -> (TimedTrace.of_list ts1, TimedTrace.of_list ts2)

  let of_nat n =
    if n < 0 then
      invalid_arg
        "TimedTracesIntervalGrade.of_nat: expected non-negative integer"
    else (TimedTrace.of_nat n, TimedTrace.of_nat n)

  let show (lo, hi) = "(" ^ TimedTrace.show lo ^ "," ^ TimedTrace.show hi ^ ")"
end

(** All available resource modules, in order of definition. The names accepted
    by the CLI's [--resources] option and listed by the web interface's grade
    selector are taken from the [name] fields of these modules. *)
let resource_grade_modules : (string * (module Grade)) list =
  [
    (TimeLowerBoundGrade.name, (module TimeLowerBoundGrade));
    (TimeUpperBoundGrade.name, (module TimeUpperBoundGrade));
    (IntervalResourceGrade.name, (module IntervalResourceGrade));
    (TimedTracesLowerBoundGrade.name, (module TimedTracesLowerBoundGrade));
    (TimedTracesUpperBoundGrade.name, (module TimedTracesUpperBoundGrade));
    (TimedTracesIntervalGrade.name, (module TimedTracesIntervalGrade));
  ]
