# Temporal Millet

Temporal Millet is a prototype programming language under development that showcases how modal types can be combined with graded effect systems to modularly specify and verify temporal properties of resources that programs manipulate. In particular, this prototype demonstrates how checking such properties can be done automatically by the means of type inference for a Hindley–Milner style type system.

The original version of Temporal Millet was implemented as part of [Joosep Tavits](https://github.com/joosepgit)'s Master's thesis at the University of Tartu ([code](https://github.com/joosepgit/temporal-millet), [thesis](https://thesis.cs.ut.ee/1c038012-af0d-444a-95dc-7ffc8b3a1f20)). This repository contains the further developments of the original prototype language, in particular it contains an extension with temporal algebraic effects and effect handlers that are guaranteed to adhere to the temporal specifications of operations.

Temporal Millet is built on top of Matija Pretnar's [Millet](https://github.com/matijapretnar/millet) language following the ideas developed by [Ahman](https://doi.org/10.1007/978-3-031-30829-1_1) and [Ahman and Žajdela](https://msfp-workshop.github.io/msfp2024/submissions/ahman+%c5%beajdela.pdf).

## How to install and run Temporal Millet?

Install dependencies by

    opam install menhir vdom ocamlformat=0.27.0

and build Millet by running (tested with OCaml >= 4.14.0)

    make

and you can clean up by running

    make clean

The repository also includes automated tests that run on every master build. To run the tests locally, run

    make test

Temporal Millet, like original Millet, gives you two options to run programs:

- The first option is a web interface, accessible at `web/index.html`, which allows you to load one of the built-in examples or enter your own program, and then interactively click through all its (non-deterministic and asynchronous) reductions or introduce external interrupts. The web interface of Temporal Millet also showcases an interactive state that tracks temporal information.<br/><br/>The web interface is also available at <https://danel.ahman.ee/temporal-millet/>.

- The second option is a command line executable run as

      ./cli.exe file1.mlt file2.mlt ...

  which loads all the commands in all the listed files and starts evaluating the given program, displaying all outgoing signals and the terminal configuration (if there is one). Non-deterministic reductions are chosen randomly and there is no option of introducing external interrupts. If you do not want to load the standard library, run Temporal Millet with the `--no-stdlib` option. If you want to see the variable context and state at the end of a program, run Temporal Millet with the `--debug` option.

## Temporal resorurces

At the core of Temporal Millet are values of modal types `[tau]a` which describe `a`-typed temporal resources that can only be used or accessed after `tau` amount of time has passed since the resource came into scope.

On the one hand, such temporal resources can be created (i.e., boxed up) with the `box tau e` command, where `e` is some `a`-typed expression that has to be well-typed in a hypothetical future `tau` time units from where `box` is called. In this case, the `box tau e` command returns a value of type `[tau]a`.

On the other hand, such temporal resources can be eliminated (i.e., unboxed) with the `unbox e`command, where `e` is some `[tau]a`-typed expression that has to have been created/brought into scope at least `tau` time units before `unbox` is called. In this case, the `unbox` command returns a value of type `a`.

Time is progressed, so that further `unbox`es become possible, by either using the `delay tau` command in your code, or by using algebraic operations discussed below, each of which takes a prescribed amount of time.

See [this example](main/examples/delay.mlt) for a demonstration how the `box`, `unbox`, and `delay` commands are supposed to be used.

## Algebraic effects and effect handlers in Temporal Millet

Temporal Millet now also supports algebraic effects and effect handlers. 

In the beginning of each Temporal Millet source file, signatures of algebraic operations can be specified using the format
```
operation OperationName : operation-input-type ~> operation-result-type # time-to-execute-operation
```
where `operation-input-type` and `operation-result-type` are Temporal Millet type expressions, and `time-to-execute-operation` is a natural number value denoting how long (how many seconds, minutes, hours etc) the given operation is supposed to execute.

These algebraic operations can be then used in the following program code using the format
```
perform OperationName operation-parameter
```
where `operation-parameter` is an expression of type `operation-input-type`. In this case, `perform OperationName operation-parameter` returns a `operation-result-type`-typed value, and the type system records that by the time the continuation of the operation call starts executing, `time-to-execute-operation` worth of extra time has passed.

As common for algebraic effects, these algebraic operation calls do not carry any meaning by themselves. To give them meaning, we have to handle them with an effect handler. In Temporal Millet, effect handlers can be defined using the format
```
let h = 
  handler
  | x -> return-case
  | OperationName p k -> operation-case
```
where `return-case` is a command that will be executed if the handled program returns a value (denoted by the variable `x`), and where
`operation-case` is a command that will be executed if the first command executed in the handled program is an operation call to operation `OperationName`.
The variable `p` is of type `operation-input-type` and denotes the parameter the operation `OperationName` was called with. The variable `k` denotes 
the continuation of the program after the call to the operation `OperationName` in question. The continuation can be resumed using the format
```
continue k with operation-result
```
where `operation-result` is an `operation-result-type`-typed expression denoting the result of handling the operation `OperationName`.

See [this example](examples/3dprint_handlers.mlt) for a worked out example of how to use algebraic effects and effect handlers in Temporal Millet on an example of correctly modelling the correct temporal usage of resources in a 3D-printing scenario.