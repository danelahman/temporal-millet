# Temporal Millet

Temporal Millet is a prototype programming language under development that showcases how modal types can be combined with graded effect systems to modularly specify and verify temporal properties of resources that programs manipulate. In particular, this prototype demonstrates how checking such properties can be done automatically by the means of type inference for a Hindley–Milner style type system.

The original version of Temporal Millet was implemented as part of Joosep Tavits's Master's thesis at the University of Tartu ([code](https://github.com/joosepgit/temporal-millet), [thesis](https://thesis.cs.ut.ee/1c038012-af0d-444a-95dc-7ffc8b3a1f20)). This repository contains the further developments of the original prototype language, in particular it contains an extension with temporal algebraic effects and effect handlers that are guaranteed to adhere to the temporal specifications of operations.

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

- The first option is a web interface, accessible at `web/index.html`, which allows you to load one of the built-in examples or enter your own program, and then interactively click through all its (non-deterministic and asynchronous) reductions or introduce external interrupts. The web interface of Temporal Millet also showcases an interactive state that tracks temporal information.

- The second option is a command line executable run as

      ./cli.exe file1.mlt file2.mlt ...

  which loads all the commands in all the listed files and starts evaluating the given program, displaying all outgoing signals and the terminal configuration (if there is one). Non-deterministic reductions are chosen randomly and there is no option of introducing external interrupts. If you do not want to load the standard library, run Temporal Millet with the `--no-stdlib` option. If you want to see the variable context and state at the end of a program, run Temporal Millet with the `--debug` option.
