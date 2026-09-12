# Temporal Millet

Temporal Millet is a prototype programming language that combines modal types
with graded effect systems to specify and verify temporal properties of the
resources programs manipulate. The properties are checked automatically by
Hindley–Milner style type inference.

The original Temporal Millet was implemented in [Joosep
Tavits](https://github.com/joosepgit)'s Master's thesis at the University of
Tartu ([code](https://github.com/joosepgit/temporal-millet),
[thesis](https://thesis.cs.ut.ee/1c038012-af0d-444a-95dc-7ffc8b3a1f20)). This
repository develops it further with (i) temporal algebraic effects and effect
handlers that are guaranteed to respect the temporal specifications of
operations, and (ii) general resource grades in place of natural-number time
grades.

Temporal Millet is built on Matija Pretnar's
[Millet](https://github.com/matijapretnar/millet) and follows the ideas of
[Ahman](https://doi.org/10.1007/978-3-031-30829-1_1) and [Ahman and
Žajdela](https://msfp-workshop.github.io/msfp2024/submissions/ahman+%c5%beajdela.pdf).

## Installing and running

Requires OCaml >= 5.0. Install the dependencies and build:

    opam install menhir vdom ocamlformat=0.28.1
    make

`make test` runs the test suite (also run by CI on every push to `main`), and
`make clean` removes the build.

There are two ways to run programs:

- **Web interface**, at `web/index.html` after building, or online at
  <https://danel.ahman.ee/temporal-millet/>. Load a built-in example or type a
  program, then step through its reductions one by one while watching the
  resource state.

- **Command line**:

      ./cli.exe file1.mlt file2.mlt ...

  loads all listed files and runs every `run` command, printing each run's
  result and final resource state. Non-deterministic choices are made at
  random. Options: `--resources <monoid>` selects the grading monoid (see
  below), `--no-stdlib` skips the standard library, and `--debug` also prints
  the typing context.

The [`examples/`](examples/) directory contains the programs available in the
web interface; each starts with the command that runs it.

## Grading monoids

Resource usage is measured in a grading monoid (an ordered monoid with some
additional structure). The monoid is not part of a source file but chosen when
the program is run: with `--resources` on the command line, e.g.

    ./cli.exe --resources time-interval examples/interval.mlt

or with the **Resource grade** selector in the web interface, which switches
automatically when a built-in example is loaded. The default is
`time-lower-bound`. Six monoids are available.

Three grade computations by time, written as integer literals such as `3` or
pairs such as `(1, 4)`:

- **`time-lower-bound`** — non-negative integers, a lower bound on the time a
  computation takes. `rho` is a sub-grade of `rho'` when `rho >= rho'`; zero is
  the *top* of the order.
- **`time-upper-bound`** — non-negative integers, an upper bound on the time a
  computation takes. `rho` is a sub-grade of `rho'` when `rho <= rho'`; zero is
  the *minimum* of the order.
- **`time-interval`** — pairs `(n, m)` with `n <= m`, a lower and an upper
  bound at once. `(n, m)` is a sub-grade of `(k, l)` when `n >= k` and
  `l >= m` (interval containment); `(0, 0)` is the minimum. See
  [`examples/interval.mlt`](examples/interval.mlt).

Three grade computations by the *timed traces* they may exhibit. These are the
timed-trace grades of the Agda formalisation `graded-temporal-resources`
(modules `Syntax/Grades/Example/Traces/Timed/*`). A timed trace is one run of a
computation, an alternation of operation events and positive delays:
`Read; 3; Send` performs `Read`, waits three ticks, and performs `Send`. A grade
is a non-empty finite set of runs, the alternatives a computation may exhibit,
written `{Read; 3; Send | Send; Send}`; grades multiply by the (non-commutative)
language product. An integer `n` abbreviates `{n}`, so the zero grade is `{0}`.
The orders trade time against operations through the runtime bounds
`within (lo, hi)` every operation declares under these monoids (see below).

- **`timed-traces-lower-bound`** — the runs a computation must *cover*. `rho`
  is a sub-grade of `rho'` when every run of `rho` covers some run of `rho'`:
  each operation performed banks its `lo` towards the delays `rho'` demands,
  but waiting never counts as performing a demanded operation. `{0}` is the
  top of the order. See
  [`examples/timed_traces_lower.mlt`](examples/timed_traces_lower.mlt).
- **`timed-traces-upper-bound`** — the runs a computation is *allowed*. `rho`
  is a sub-grade of `rho'` when every run of `rho` fits inside some run of
  `rho'`: a delay in `rho'` pays for operations of `rho` at their `hi`, but
  waiting never counts as performing an operation the bound asks for. `{0}` is
  the minimum of the order. See
  [`examples/timed_traces_upper.mlt`](examples/timed_traces_upper.mlt).
- **`timed-traces-interval`** — pairs `({...}, {...})` of a lower bound
  (coverage order, reading `lo`) and an upper bound (allowance order, reading
  `hi`), compared componentwise. `{...}` abbreviates the pair of a set with
  itself, `n` abbreviates `({n}, {n})`, and `(n, m)` abbreviates
  `({n}, {m})`. `({0}, {0})` is neither the top nor the minimum. See
  [`examples/timed_traces_interval.mlt`](examples/timed_traces_interval.mlt).

## Temporal resources

A value of the modal type `[rho]a` is an `a`-typed resource that may be used
only once the grade `rho` has been accumulated since it was created: an amount
of time, a sequence of operations, or whatever the chosen monoid measures.

- `box rho e` creates a resource of type `[rho]a`. The expression `e` is typed
  in the hypothetical future in which the accumulated grade has grown by `rho`.
- `unbox e` opens a resource `e : [rho]a`, yielding an `a`. It is allowed only
  if the grade accumulated since `e` was boxed is at least `rho` in the
  sub-grade order.
- `delay tau` advances the accumulated grade by `tau`. Operation calls (below)
  advance it by the grade of the operation.

See [`examples/delay.mlt`](examples/delay.mlt).

## Eternal types

A type is *eternal* if its values stay valid however much grade accumulates
after they are bound. Eternal are the base types (integers, strings, booleans,
floats, unit), tuples of eternal types, and algebraic types all of whose
constructor arguments are eternal. Function types, handler types, and box types
`[rho]a` are never eternal, and type variables are currently not eternal
either.

Referencing a local variable `x : a` generates the constraint "`a` is eternal,
or the grade accumulated since `x` was bound is a sub-grade of zero". Under
`time-lower-bound` and `timed-traces-lower-bound` zero is the top of the order,
so the constraint always holds and any local variable may be used at any later
point. Under the other monoids a local variable of a non-eternal type must be
used before any `delay` or operation call has happened since it was bound.

A type definition can be declared non-eternal regardless of its structure:

```
noneternal type epoxy = Epoxy
```

This makes `epoxy`, and every type containing it, non-eternal. The keyword
prefixes a whole `type ... and ...` group. It is allowed on algebraic types
only, since type aliases are unfolded before eternality is checked. In the
timed-trace examples, mixed epoxy is such a resource: once unboxed it has to be
used at once.

Eternality constraints are not propagated out of top-level definitions onto
the type variables of their generalised types; a top-level definition
typechecks only if all its constraints are satisfied.

## Algebraic effects and effect handlers

Operations are declared at the top of a source file:

```
operation OperationName : input-type ~> result-type # grade
```

The grade records the resource usage of one call. Under the timed-trace
monoids the declaration must also state the operation's runtime bounds,

```
operation OperationName : input-type ~> result-type # grade within (lo, hi)
```

the least and greatest number of ticks a call may take (`within n` is short for
`within (n, n)`). The bounds are the cost model of the trace orders:
`timed-traces-lower-bound` reads `lo`, `timed-traces-upper-bound` reads `hi`,
and `timed-traces-interval` reads both. They must agree with the grade: `lo`
may not exceed the fastest run the grade allows and `hi` must cover the
slowest, each event costed at the matching end of its own bounds. An atomic
operation such as `Heat : unit ~> unit # {Heat} within (1, 2)` is trivially
consistent; `Send : string ~> unit # {Tx | Tx; Tx} within (2, 6)` is consistent
given `Tx within (2, 3)`; a self-referential `Send # {Send | Send; Send}` never
is, since its retry costs twice the bound of `Send` itself, so retries are
expressed through a smaller operation. The events of a grade must be declared
operations, or the operation being declared. Under the time monoids no bounds
are declared, since the grade already is the bound.

An operation is called with

```
perform OperationName argument
```

which returns a `result-type` value and advances the accumulated grade by the
operation's grade.

Calls are given meaning by handlers:

```
let h =
  handler
  | x -> return-case
  | OperationName p k -> operation-case
  | ...
```

The return case runs when the handled program returns a value `x`. An
operation case runs when the handled program calls the operation; `p` is the
argument and `k` the continuation, resumed with `continue k with result`.
Operations without a case are forwarded to the enclosing handler.

An operation case need not have exactly the grade of the operation: it
suffices that its grade is a sub-grade of the operation's grade composed with
that of the continuation. So `PrintModel : model ~> fresh # {Heat; Extrude;
Cool} within (6, 9)` may be handled by performing `Heat`, `Extrude` and `Cool`
in that order and continuing, while another order is rejected:

```
Comparing resource inequality {Cool; Extrude; Heat} <= {Heat; Extrude; Cool} failed
```

Under the time monoids the same rule lets a case delay longer than the
operation's grade under `time-lower-bound`, and shorter under
`time-upper-bound`.

See [`examples/handlers.mlt`](examples/handlers.mlt) and
[`examples/3dprint_handlers.mlt`](examples/3dprint_handlers.mlt).

### Default implementations

An operation may be given one default implementation, after its declaration:

```
default OperationName p = t
```

The default runs only when a call reaches the top level unhandled, that is,
after every enclosing handler has forwarded it; a handled call never uses it,
and a call without a default stops the run. The body `t` runs in place of the
call and its result goes to the continuation.

A default is checked against the operation's runtime bounds rather than its
grade: the body must have a sub-grade of `{lo}` under
`timed-traces-lower-bound`, of `{hi}` under `timed-traces-upper-bound`, and of
`({lo}, {hi})` under `timed-traces-interval`. Under the time monoids it is
checked against the operation's grade. The grade itself cannot be required,
since realising `{Heat}` would mean performing `Heat` again. So

```
operation Heat : unit ~> unit # ({Heat}, {Heat}) within (1, 2)
default Heat () = delay 1
```

is accepted under `timed-traces-interval` because `({1}, {1}) <= ({1}, {2})`,
while `delay 6` is rejected:

```
Comparing resource inequality ({6},{6}) <= ({1},{2}) failed
```

Under the trace monoids only *atomic* operations, graded by the single run of
themselves such as `Heat # {Heat}`, may have defaults; a compound operation
such as `PrintModel # {Heat; Extrude; Cool}` is meant to be handled in terms
of the operations it names. A default may itself perform operations, which are
handled or defaulted in turn; a default that performs its own operation
typechecks but never terminates.

## Editor support

`editors/vscode/` contains a minimal VS Code extension with syntax highlighting
for `.mlt` files. Package and install it with

    make vscode-extension

or by hand with

    cd editors/vscode
    npx --yes @vscode/vsce package
    code --install-extension vscode-temporal-millet-*.vsix --force

then restart VS Code. Uninstall with

    code --uninstall-extension temporal-millet.vscode-temporal-millet

To work on the extension itself, open `editors/vscode` in VS Code and press F5
for an Extension Development Host with the extension loaded.

## License

Temporal Millet is released under the MIT license (see [`LICENSE`](LICENSE)).
It is derived from Matija Pretnar's
[Millet](https://github.com/matijapretnar/millet) and from Joosep Tavits's
[original Temporal Millet](https://github.com/joosepgit/temporal-millet), both
MIT licensed; their copyright notices are retained in `LICENSE`.
