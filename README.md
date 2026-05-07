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

## Temporal resources

At the core of Temporal Millet are values of modal types `[rho]a` which describe
`a`-typed temporal resources that can only be used or accessed within `rho` 
amount of computation, e.g., only within a time interval modelled by the `rho`.

On the one hand, such temporal resources can be created (i.e., boxed up) with
the `box rho e` command, where `e` is some `a`-typed expression that has to be
well-typed in a hypothetical future within a `rho` amount of computation from
where `box` is called. In this case, `box rho e` returns a value of type `[rho]a`.

On the other hand, such temporal resources can be eliminated (i.e., unboxed)
with the `unbox e` command, where `e` is some `[rho]a`-typed expression that has
to have been created/brought into scope within `rho` amount of computation before 
`unbox` is called. In this case, the `unbox` command returns a value of type `a`.

Time is progressed, so that further `unbox` es become possible, by either using
the `delay tau` command in your code, to delay the execution of the program's
continuation by `tau` time units, or by making calls to algebraic operations as
discussed below, each of which performs a prescribed grade amount of computation.

See [this](main/examples/delay.mlt) example for a demonstration how the `box`,
`unbox`, and `delay` commands are supposed to be used.

## Algebraic effects and effect handlers in Temporal Millet

Temporal Millet now also supports algebraic effects and effect handlers. 

In the beginning of each Temporal Millet source file, signatures of algebraic
operations can be specified using the format
```
operation OperationName : operation-input-type ~> operation-result-type # operation-grade
```
where `operation-input-type` and `operation-result-type` are Temporal Millet
type expressions, and `operation-grade` is a grade specifying the operation's
computational behaviour (e.g., how many seconds, minutes, hours etc the given
operation is supposed to execute.)

These algebraic operations can be then used in the following program code using
the format
```
perform OperationName operation-parameter
```
where `operation-parameter` is an expression of type `operation-input-type`. In
this case, `perform OperationName operation-parameter` returns a
`operation-result-type`-typed value, and the type system records that by the
time the continuation of the operation call starts executing,
`operation-grade` worth of extra computation has been performed.

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
