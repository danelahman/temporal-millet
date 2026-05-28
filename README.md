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
  Temporal Millet with the `--debug` option.

## Grading monoids

A Temporal Millet source file can optionally begin with a `resources`
declaration that selects the grading monoid (an ordered monoid satisfying some
additional properties) used to track resource usage throughout the file:

```
resources grade-name
```

If no such declaration is present, the `time-lower-bound` grading monoid is used
by default. Three grading monoids are currently available:

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

See [this](examples/handlers.mlt) and [this](examples/3dprint_handlers.mlt)
example for a worked out examples of how to use algebraic effects and effect
handlers in Temporal Millet.

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
