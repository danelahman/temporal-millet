# Temporal Millet

Temporal Millet is a prototype programming language under development that
showcases how modal types can be combined with graded effect systems to
modularly specify and verify temporal properties of resources that programs
manipulate. In particular, this prototype demonstrates how checking such
properties can be done automatically by the means of type inference for a
Hindley–Milner style type system.

The original version of Temporal Millet was implemented as part of [Joosep
Tavits](https://github.com/joosepgit)'s Master's thesis at the University of
Tartu ([code](https://github.com/joosepgit/temporal-millet),
[thesis](https://thesis.cs.ut.ee/1c038012-af0d-444a-95dc-7ffc8b3a1f20)). This
repository contains the further developments of the original prototype language,
in particular, it contains (i) an extension with temporal algebraic effects and
effect handlers that are guaranteed to adhere to the temporal specifications of
operations, and (ii) an extension from just natural-number (time) grades to
general resource grades.

Temporal Millet is built on top of Matija Pretnar's
[Millet](https://github.com/matijapretnar/millet) language following the ideas
developed by [Ahman](https://doi.org/10.1007/978-3-031-30829-1_1) and [Ahman and
Žajdela](https://msfp-workshop.github.io/msfp2024/submissions/ahman+%c5%beajdela.pdf).

## How to install and run Temporal Millet?

Install dependencies by

    opam install menhir vdom ocamlformat=0.28.1

and build Millet by running (tested with OCaml >= 4.14.0)

    make

and you can clean up by running

    make clean

The repository also includes automated tests that run on every master build. To
run the tests locally, run

    make test

Temporal Millet, like original Millet, gives you two options to run programs:

- The first option is a web interface, accessible at `web/index.html`, which
  allows you to load one of the built-in examples or enter your own program, and
  then interactively click through all its (non-deterministic and asynchronous)
  reductions or introduce external interrupts. The web interface of Temporal
  Millet also showcases an interactive state that tracks temporal
  information.<br/><br/>The web interface is also available at
  <https://danel.ahman.ee/temporal-millet/>.

- The second option is a command line executable run as

      ./cli.exe file1.mlt file2.mlt ...

  which loads all the commands in all the listed files and starts evaluating the
  given program, displaying all outgoing signals and the terminal configuration
  (if there is one). Non-deterministic reductions are chosen randomly and there
  is no option of introducing external interrupts. If you do not want to load
  the standard library, run Temporal Millet with the `--no-stdlib` option. If
  you want to see the variable context and state at the end of a program, run
  Temporal Millet with the `--debug` option. To pick the grading monoid the
  program is checked against, use the `--resources` option (see below).

## Grading monoids

The grading monoid (an ordered monoid satisfying some additional properties)
used to track resource usage is not part of a source file — it is chosen when
the program is run:

- On the command line, with the `--resources` option, e.g.

      ./cli.exe --resources time-interval examples/interval.mlt

- In the web interface, with the **Resource grade** selector. Loading one of the
  built-in examples switches the selector to the grading monoid that example is
  written for; you are free to change it afterwards for your own programs.

In both cases the default is the `time-lower-bound` grading monoid. Six
grading monoids are currently available:

- **`time-lower-bound`** — grades are non-negative integers representing
  discrete time units tracking the lower bound of the time-cost of computations.
  The zero grade is the *top* element of the sub-grade order: grade `rho` is
  considered a sub-grade of grade `rho'` when `rho >= rho'`. Grades of this kind
  are written as plain integer literals, e.g. `3`.

- **`time-upper-bound`** — grades are non-negative integers representing
  discrete time units tracking the upper bound of the time-cost of computations.
  The zero grade is the *minimal* element of the sub-grade order: grade `rho`
  is considered a sub-grade of grade `rho'` when `rho <= rho'`. Grades of this
  kind are written as plain integer literals, e.g. `3`.

- **`time-interval`** — grades are pairs of non-negative integers `(n, m)` with
  `n <= m`, representing time intervals describing both the lower and upper
  bounds of the time-cost of computations. The zero grade `(0, 0)` is the
  *minimal* element of the sub-grade order: grade `(n, m)` is considered a
  sub-grade of `(k, l)` when `n >= k` and `l >= m` (i.e. the interval is
  contained within the other). Grades are written as pair literals, e.g. `(1,
  4)`. See [this](examples/interval.mlt) example for a demonstration of
  time-interval grades.

The remaining three grading monoids grade computations by the *timed traces*
they may exhibit rather than by time alone. They are the timed trace grades of
the Agda formalisation `graded-temporal-resources`, whose
`Syntax/Grades/Example/Traces/Timed/*` modules are the reference definitions for
everything described below. A *timed trace* is one possible run of a
computation, written as an alternation of operation events and delays, e.g.
`Read; 3; Send` is the run that performs `Read`, waits three time units, and
performs `Send`. The events are the operations declared in the source file (see
below) and the delays are positive integers. A grade is a non-empty finite set
of such runs, read as the alternatives a computation may exhibit, and written
`{Read; 3; Send | Send; Send}`; grades are multiplied by the language product of
these sets, which, unlike the product of the time grades, is not commutative. A
plain integer literal `n` is short for `{n}`, the set containing the single run
that only waits, so in particular the zero grade is `{0}`. The sub-grade orders
trade time against operations using the runtime bounds `within (lo, hi)` that
every operation has to declare under these grading monoids (see below): the
coverage order reads the lower end `lo` and the allowance order reads the upper
end `hi`.

- **`timed-traces-lower-bound`** — grades are non-empty finite sets of timed
  traces describing the runs a computation is required to cover. The sub-grade
  order is the *coverage* order: grade `rho` is considered a sub-grade of grade
  `rho'` when every run of `rho` covers some run of `rho'`, where performing an
  operation banks the lower end `lo` of its runtime bounds towards the delays
  `rho'` demands. In other words, operations bank their lower bound to cover
  required delays, but waiting is never a way of performing a demanded
  operation. The zero grade `{0}` is the *top* element of the sub-grade order.
  See [this](examples/timed_traces_lower.mlt) example for a demonstration.

- **`timed-traces-upper-bound`** — grades are non-empty finite sets of timed
  traces describing the runs a computation is permitted to exhibit. The
  sub-grade order is the *allowance* order: grade `rho` is considered a
  sub-grade of grade `rho'` when every run of `rho` fits inside some run of
  `rho'`, where a delay of `rho'` pays for the operations of `rho`, each at the
  upper end `hi` of its runtime bounds. In other words, time in the bound buys
  operations, but waiting is never a way of performing an operation the bound
  asks for. The zero grade `{0}` is the *minimal* element of the sub-grade
  order. See [this](examples/timed_traces_upper.mlt) example for a
  demonstration.

- **`timed-traces-interval`** — grades are pairs `({...}, {...})` of non-empty
  finite sets of timed traces, the first component a lower bound ordered by the
  coverage order (reading `lo`) and the second component an upper bound
  ordered by the allowance order (reading `hi`); grade `rho` is considered a
  sub-grade of grade `rho'` when both of its components are. A single set
  `{...}` abbreviates the pair of that set with itself, a plain integer `n`
  abbreviates `({n}, {n})`, and a pair of integers `(n, m)` abbreviates
  `({n}, {m})`. The zero grade `({0}, {0})` is neither the minimal nor the top
  element of the sub-grade order. See [this](examples/timed_traces_interval.mlt)
  example for a demonstration.

## Temporal resources

At the core of Temporal Millet are values of modal types `[rho]a` which describe
`a`-typed resources whose accessibility is governed by the grade `rho`. Depending
on the chosen grading monoid, `rho` may represent, for example, the amount of
time that must have elapsed, the sequence of operations that must have been
performed, or any other monoidal measure (with certain additional properties) 
of computational progress, before the resource can be accessed.

On the one hand, such resources can be created (i.e., boxed up) with the
`box rho e` command, where `e` is some `a`-typed expression that has to be
well-typed in a hypothetical future in which the accumulated grade has increased
by `rho` from the point where `box` is called. In this case, `box rho e` returns
a value of type `[rho]a` representing a temporal resource.

On the other hand, such resources can be eliminated (i.e., unboxed) with the
`unbox e` command, where `e` is some `[rho]a`-typed expression. The `unbox`
command can only be used once the accumulated grade has advanced by at least
`rho` (in the sub-grade order of the chosen grading monoid) since the resource
was boxed. In this case, the `unbox` command returns a value of type `a`.

The accumulated grade is advanced, so that further `unbox`es become possible, by
either using the `delay tau` command in your code, which explicitly advances the
accumulated grade by `tau`, or by making calls to algebraic effect operations as
discussed below, each of which contributes a prescribed grade to the grade
accumulated in the program context.

See [this](examples/delay.mlt) example for a demonstration how the `box`,
`unbox`, and `delay` commands are supposed to be used.

## Eternal types

A type is called *eternal* if values of that type remain valid regardless of
how much grade has been accumulated since they were introduced. Concretely, a
type is eternal when:

- it is a base constant type that is considered eternal (e.g., integers,
  booleans, and unit, but not, say, file handles --- at the moment all supported
  base constant types are treated as eternal by the typechecker);
- it is a tuple all of whose component types are eternal;
- it is a user-defined algebraic type all of whose constructor argument types
  are eternal.

Function types (`a -> b`), handler types (`a # rho1 => b # rho2`), and temporal
box (resource) types (`[rho]a`) are never eternal because they might contain
computations or resources that are temporally sensitive and can only be used "now".

When a local variable `x` of type `a` is referenced, the type system generates
an *eternal-or-inequality* constraint: either `a` is eternal, or the grade
accumulated in the context since `x` was bound is a sub-grade of zero. The
practical effect of this constraint depends on the chosen grading monoid:

- For the **`time-lower-bound`** monoid, where zero is the *top* element of the sub-grade
  order, the inequality `accumulated_grade ≤ zero` holds trivially for every
  non-negative integer grade. Consequently, local variables of **any** type may
  be freely referenced at any later point in the computation regardless of how
  much time has elapsed.

- For the **`time-upper-bound`** monoid, where zero is the *minimal* element of
  the sub-grade order, the inequality `accumulated_grade ≤ zero` holds only when
  the accumulated grade is exactly `0` (i.e. no grade has been accumulated at
  all). This means that local variables whose types are not eternal must be
  used in the very same time-step in which they were bound — before any `delay`
  or operation calls have occurred — while variables with eternal types may be
  referenced freely at any later point.

- For the **`time-interval`** monoid, where zero `(0, 0)` is the *minimal*
  element of the sub-grade order, the inequality `accumulated_grade ≤ (0, 0)`
  holds only when the accumulated grade is exactly `(0, 0)` (i.e. no grade
  has been accumulated at all). This means that local variables whose types
  are not eternal must be used in the very same time-step in which they were
  bound — before any `delay` or operation calls have occurred — while
  variables with eternal types may be referenced freely at any later point.

A type definition can also be declared *non-eternal* explicitly, by prefixing
it with the `noneternal` keyword:
```
noneternal type epoxy = Epoxy
```
This makes `epoxy` non-eternal no matter what its structure says, and, through
the structural rules above, also every type that contains it (a tuple with an
`epoxy` component, or an algebraic type one of whose constructors takes an
`epoxy`). The keyword prefixes a whole `type ... and ...` group and marks every
definition in it. It is only allowed on algebraic (sum) types: a type alias such
as `noneternal type seconds = int` is rejected, because aliases are transparent
and are unfolded by the unifier before the eternality check ever sees them.

The declaration is what lets one model a resource that is structurally
"harmless" but nevertheless time-sensitive: mixed epoxy does not keep, so once
unboxed it has to be used right away. Like every eternality constraint, it only
bites under the monoids in which zero is not the top of the sub-grade order
(`time-upper-bound`, `time-interval`, and the timed-trace upper-bound and
interval monoids): there a local variable of type `epoxy` must be used in the
same time-step in which it was bound. Under `time-lower-bound` the declaration
has no observable effect. See the
[timed trace examples](examples/timed_traces_upper.mlt) for a use.

Currently type variables are not considered eternal, and no eternality
constraints are propagated out of top-level functions as constraints on type
variables appearing in the computed generalised polymorphic types. A top-level
definition is only successfully typechecked if all generated eternality
constraints are satisfied (or the accompanying inequalities are satisfied).

## Algebraic effects and effect handlers in Temporal Millet

Temporal Millet now also supports algebraic effects and effect handlers. 

In the beginning of each Temporal Millet source file, signatures of algebraic
operations can be specified using the format
```
operation OperationName : operation-input-type ~> operation-result-type # operation-grade
```
where `operation-input-type` and `operation-result-type` are Temporal Millet
type expressions, and `operation-grade` is a grade specifying the resource usage
incurred by the operation (e.g., how much time, how many steps, or what sequence
of sub-operations the given operation is supposed to involve).

Under the timed-trace grading monoids, the signature of an operation must in
addition declare the runtime bounds of the operation, using the format
```
operation OperationName : operation-input-type ~> operation-result-type # operation-grade within (lo, hi)
```
where `lo` and `hi` are the least and the greatest number of time units a call
to the operation may take, with `within n` being short for `within (n, n)`.
These bounds are the cost model against which the orders of the trace grades
trade time for operations: the coverage order of `timed-traces-lower-bound`
reads `lo`, the allowance order of `timed-traces-upper-bound` reads `hi`, and
`timed-traces-interval` reads both. The declared bounds are themselves checked
for consistency with the grade of the operation: `lo` may not exceed the
duration of the fastest run the grade allows and `hi` must cover the duration of
its slowest run, where a run costs its delays plus, for each of its events, the
matching end of the runtime bounds of the operation named — the operation's own
bounds being used for its own events. An atomic operation, graded by the
single run that is itself, is therefore trivially consistent, as in `Heat #
{Heat} within (1, 2)`, while `Send : string ~> unit # {Tx | Tx; Tx} within (2,
6)` is consistent exactly because a `Tx within (2, 3)` takes between two and
three ticks and `Send` promises one or two of them. A self-referential grade
such as `Send # {Send | Send; Send}` is on the other hand never consistent,
since its retrying run costs twice the upper bound of `Send` itself, so an
operation that may be retried has to be expressed through a smaller operation,
as in `{Tx | Tx; Tx}`, rather than through itself. Under the time grading
monoids the bounds must not be declared, because there the grade of an operation
already is its runtime bound.

The events that a trace literal is built from are operation names, and each of
them must be the name of an operation that has already been declared, or the
name of the operation being declared. The latter is what makes grades such as `#
{Send | Send; Send}` possible, promising a send that may be retried once.

These algebraic operations can be then used in the following program code using
the format
```
perform OperationName operation-parameter
```
where `operation-parameter` is an expression of type `operation-input-type`. In
this case, `perform OperationName operation-parameter` returns a
`operation-result-type`-typed value, and the type system records that the
accumulated grade at the point where the continuation starts executing has
increased by `operation-grade`.

As is common for algebraic effects, these algebraic operation calls do not carry
any meaning by themselves. To give them meaning, we have to handle them with an
effect handler. In Temporal Millet, effect handlers can be defined using the
format
```
let h = 
  handler
  | x -> return-case
  | OperationName-1 p k -> operation-case-1
  | ...
  | OperationName p k -> operation-case
  | ...
  | OperationName-n p k -> operation-case-n
```
where `return-case` is a command that will be executed if the handled program
returns a value (denoted by the variable `x`), which is followed by operation
cases for one or more of the operations declared in the beginning of the source
file. 

For instance, `operation-case` is a command that will be executed if the first
command executed in the handled program is an operation call to operation
`OperationName`. The variable `p` is of type `operation-input-type` and denotes
the parameter the operation `OperationName` was called with. The variable `k`
denotes the continuation of the program after the call to the operation
`OperationName` in question. The continuation `k` can be resumed in an operation
case using the format
```
continue k with operation-result
```
where `operation-result` is an `operation-result-type`-typed expression denoting
the result of handling the operation `OperationName`.

Operations that do not have their corresponding operation cases given in a
handler are handled by themselves by the given handler.

The grade of an operation case does not have to match the grade of the operation
it handles exactly — it suffices that it is a sub-grade of `operation-grade`
composed with the grade of the continuation `k` (sub-effecting). For instance,
the operation
```
operation PrintModel : model ~> fresh # {Heat; Extrude; Cool} within (6, 9)
```
can be handled by an operation case that performs `Heat`, `Extrude`, and `Cool`,
in that order, and then continues, because the grade accumulated by the three
calls is a sub-grade of the grade of `PrintModel`. Performing the same three
operations in another order is instead rejected with a message such as
```
Comparing resource inequality {Cool; Extrude; Heat} <= {Heat; Extrude; Cool} failed
```
Under the time grading monoids the same rule means that an operation case may
delay for longer than the grade of the operation prescribes when the monoid is
`time-lower-bound`, and for less than it prescribes when the monoid is
`time-upper-bound`.

See [this](examples/handlers.mlt) and [this](examples/3dprint_handlers.mlt)
example for a worked out examples of how to use algebraic effects and effect
handlers in Temporal Millet.

### Default implementations of operations

An operation can also be given a *default implementation*, using the format
```
default OperationName p = t
```
where `p` is a pattern of type `operation-input-type` and `t` is a command of
type `operation-result-type`. An operation may be given at most one default
implementation, and only after it has been declared.

A default implementation fires only when a call to the operation reaches the top
level unhandled, that is, once it has been forwarded out of every enclosing
`let ... in` and `handle ... with`. The body of the default then runs in place of
the operation call and its result is passed to the continuation, just as the
result of an operation case would be. An operation that *is* handled therefore
never uses its default, and operations that have no default still stop the run
when they reach the top level.

Because a default *is* the implementation of the operation, it cannot be checked
the way an operation case of a handler is. An operation case for `Heat` may spend
the grade of `Heat` itself, since by the time it runs the operation has already
been performed, whereas a default has nothing of the sort to spend — under the
trace grading monoids the only way to realise `{Heat}` would be to perform `Heat`
again. A default is checked against the runtime bounds of its operation instead:
with
```
operation OperationName : operation-input-type ~> operation-result-type # operation-grade within (lo, hi)
```
the body of the default must have a grade that is a sub-grade of `{lo}` under
`timed-traces-lower-bound`, of `{hi}` under `timed-traces-upper-bound`, and of
`({lo}, {hi})` under `timed-traces-interval`. Under the time grading monoids no
bounds are declared, because there the grade of an operation already is its
runtime bound, and the default is checked against the operation grade itself.

For instance, the default implementation
```
operation Heat : unit ~> unit # ({Heat}, {Heat}) within (1, 2)

default Heat () = delay 1
```
is accepted under `timed-traces-interval`, because the grade `({1}, {1})` of its
body is a sub-grade of `({1}, {2})`. Delaying for six ticks instead would be
rejected with a message such as
```
Comparing resource inequality ({6},{6}) <= ({1},{2}) failed
```

Under the trace grading monoids a default implementation may moreover only be
given for an *atomic* operation, one whose grade is the single run consisting of
the operation itself, such as `Heat # {Heat}`. A compound operation such as
`PrintModel # {Heat; Extrude; Cool}` names the operations it decomposes into, and
is meant to be given meaning by a handler in terms of them; asking for a default
for it is rejected with
```
a default implementation may only be given for an atomic operation, but the grade of PrintModel is {Heat; Extrude; Cool}; handle it with a handler in terms of the operations it names
```

The body of a default may itself perform operations, which are then handled or
defaulted in turn — this is how a default that runs at the top level can still
make use of the rest of the program's effects. In particular, a default that
performs its own operation typechecks, but never terminates, in the same way as
any other non-terminating program.

## Logo

The logo of Temporal Millet, in SVG and PNG and in light, dark, and
single-colour variants, lives in [`web/logo/`](web/logo/) together with a short
description of its design and colours. The web interface uses it in its header
and as its favicon.

## Editor support

A minimal VS Code extension providing OCaml-style syntax highlighting for
`.mlt` source files is included in `editors/vscode/`. 

To package and install it locally in one step, run

    make vscode-extension

from the repository root. This packages the extension as a `.vsix` with
`vsce` and installs it via the `code` CLI, overwriting any previously
installed version. 

Equivalent manual steps for packaging and installing are

    cd editors/vscode
    npx --yes @vscode/vsce package
    code --install-extension vscode-temporal-millet-*.vsix --force

Then fully restart VS Code and open any `.mlt` file. 

To uninstall, run

    code --uninstall-extension temporal-millet.vscode-temporal-millet

While iterating on the extension itself, an alternative is to open the
extension folder in VS Code (`code editors/vscode`) and press F5 — this
launches an Extension Development Host window with the extension preloaded,
without needing to repackage on every change.
