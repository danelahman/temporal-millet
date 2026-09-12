(** Timed traces and non-empty finite sets of them, the carrier of the
    timed-trace grading monoids.

    A timed trace is one run of a computation as it is observed from outside: an
    alternation of operation events and positive delays. A grade is a set of
    such runs, read disjunctively, multiplied by the language product. The two
    orders below — allowance (an upper bound: "every run fits inside the bound")
    and coverage (a lower bound: "the run covers the guarantee") — are direct
    transcriptions of [Syntax/Grades/Traces/{Allowance,Coverage}/Core.agda] of
    the [graded-temporal-resources] formalisation, and the set lifts of
    [Syntax/Grades/Traces/OrderLift.agda].

    {2 Canonical representation}

    Grade values are compared with the structural [=] and [compare] all over the
    typechecker, the context and the pretty-printer, so every function here that
    produces a [trace] or a [traces] returns the canonical normal form: a trace
    contains no [Wait 0] and no two adjacent [Wait]s, and a set of traces is
    sorted and duplicate-free. Anything building a value of these types outside
    this module must go through {!normalise} and {!of_list}. *)

type event =
  | Ev of string  (** an operation event, named by its surface name *)
  | Wait of int  (** a delay; in normal form its duration is at least 1 *)

type trace = event list
(** One run. Normal form: no [Wait 0], no two adjacent [Wait]s. *)

type traces = trace list
(** A non-empty finite set of runs. Normal form: sorted and duplicate-free, as
    produced by [List.sort_uniq compare]. *)

(** [normalise evs] is the trace denoted by the raw event sequence [evs]: zero
    delays are dropped and adjacent delays are merged. This is the [Trace⁻]
    invariant of the formalisation, and what makes [of_nat (m + n)] equal to the
    product of [of_nat m] and [of_nat n] on the nose. *)
let normalise evs =
  let rec go acc = function
    | [] -> List.rev acc
    | Wait n :: rest when n <= 0 -> go acc rest
    | Wait n :: rest -> (
        match acc with
        | Wait m :: acc' -> go (Wait (m + n) :: acc') rest
        | _ -> go (Wait n :: acc) rest)
    | (Ev _ as e) :: rest -> go (e :: acc) rest
  in
  go [] evs

(** [concat s t] is the trace monoid [_∙_]: the trailing delay of [s] is merged
    with the leading delay of [t]. *)
let concat s t = normalise (s @ t)

(** [of_list ts] is the canonical set of traces denoted by the raw list [ts]. *)
let of_list ts = List.sort_uniq compare (List.map normalise ts)

(** [product p q] is the language product: every run of [p] sequenced with every
    run of [q]. *)
let product p q =
  List.sort_uniq compare
    (List.concat_map (fun s -> List.map (fun t -> concat s t) q) p)

(** [of_nat n] is the singleton set containing the pure delay of duration [n];
    [of_nat 0] is the unit [{ε}]. *)
let of_nat n = [ normalise [ Wait n ] ]

(** [allowance cost k s t] decides [s ≼ᵃ[k] t]: the bound [t] permits the run
    [s], given [k] units of budget already banked. Budget comes from the bound's
    delays and is spent on the run's delays and on operations the bound does not
    name, at their [cost]; a matched operation resets the budget, so slack the
    bound offers before an operation it names is spent before that operation or
    not at all.

    Each rule shrinks [s] or [t], so the plain backtracking search terminates.
    The [when] guards are the backtracking: a failing guard falls through to the
    next rule, which is exactly the order the Agda constructors are tried in. *)
let rec allowance cost k s t =
  match (s, t) with
  | [], _ -> true (* nil *)
  | Ev o :: s', Ev o' :: t' when o = o' && allowance cost 0 s' t' ->
      true (* keep: the budget does not cross a match *)
  | _, Ev _ :: t' when allowance cost k s t' -> true (* skip-op *)
  | _, Wait e :: t' when allowance cost (k + e) s t' -> true (* bank-delay *)
  | Wait d :: s', _ when d <= k && allowance cost (k - d) s' t ->
      true (* use-delay *)
  | Ev o :: s', _ when cost o <= k && allowance cost (k - cost o) s' t ->
      true (* use-op *)
  | _ -> false

(** [coverage cost k s t] decides [s ≼ᶜ[k] t]: the run [t] covers the guarantee
    [s], given [k] units of slack already banked. Slack comes from whatever [t]
    did that [s] did not demand — its unmatched operations at their [cost] and
    its delays at their length — and is spent only on delays [s] demands.
    Nothing but the operation itself discharges a demand for an operation, which
    is what a guarantee wants and what makes this order not the converse of
    {!allowance}. *)
let rec coverage cost k s t =
  match (s, t) with
  | [], _ -> true (* nil *)
  | Ev o :: s', Ev o' :: t' when o = o' && coverage cost 0 s' t' ->
      true (* keep: the slack does not cross a match *)
  | _, Ev o :: t' when coverage cost (k + cost o) s t' -> true (* pay-op *)
  | _, Wait e :: t' when coverage cost (k + e) s t' -> true (* del-delay *)
  | Wait d :: s', _ when d <= k && coverage cost (k - d) s' t ->
      true (* use-delay *)
  | _ -> false

(** [upper_bound_le cost p q] is the Hoare lift of the allowance order: every
    bound listed by [p] stays within some bound listed by [q]. This is the
    sub-grade order of the upper-bound (right-sided) grade. *)
let upper_bound_le cost p q =
  List.for_all (fun s -> List.exists (fun t -> allowance cost 0 s t) q) p

(** [lower_bound_le cost p q] is the Smyth lift of the coverage order: every
    guarantee listed by [p] has an easier one listed by [q]. Note the direction
    — the universal quantifier runs over [p] but the witness is compared the
    other way round ([t ≼ᶜ s]), as in [xs ≽ˢ ys = liftˢ _≽_ ys xs]. This is the
    sub-grade order of the lower-bound (left-sided) grade. *)
let lower_bound_le cost p q =
  List.for_all (fun s -> List.exists (fun t -> coverage cost 0 t s) q) p

(** [duration cost t] is the time the run [t] takes: every delay counts its
    length and every operation event counts [cost]. Reading [cost] as the lower
    end of the runtime bounds gives the fastest the run can be, and as the upper
    end the slowest; this is the duration morphism of the timed trace modules of
    the formalisation. *)
let duration cost =
  List.fold_left (fun d -> function Ev o -> d + cost o | Wait n -> d + n) 0

(** [min_duration cost p] is the duration of the fastest run of the (non-empty)
    set [p], as [minSetDuration] of the formalisation. *)
let min_duration cost = function
  | [] -> invalid_arg "TimedTrace.min_duration: empty set of traces"
  | t :: p ->
      List.fold_left (fun d t -> min d (duration cost t)) (duration cost t) p

(** [max_duration cost p] is the duration of the slowest run of the (non-empty)
    set [p], as [setDuration] of the formalisation. *)
let max_duration cost = function
  | [] -> invalid_arg "TimedTrace.max_duration: empty set of traces"
  | t :: p ->
      List.fold_left (fun d t -> max d (duration cost t)) (duration cost t) p

(** [events p] lists, sorted and without repetitions, the operation names
    mentioned anywhere in [p]. *)
let events p =
  List.sort_uniq compare
    (List.concat_map
       (List.filter_map (function Ev o -> Some o | Wait _ -> None))
       p)

let show_event = function Ev o -> o | Wait n -> string_of_int n

(** [show_trace t] prints a run as [Read; 3; Send]; the empty run prints as [0],
    the delay of duration zero it denotes. *)
let show_trace = function
  | [] -> "0"
  | t -> String.concat "; " (List.map show_event t)

(** [show p] prints a set of runs as [{Read; 3; Send | Send; Send}]. *)
let show p = "{" ^ String.concat " | " (List.map show_trace p) ^ "}"
