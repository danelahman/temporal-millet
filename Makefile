default:
	dune build

format:
	dune build @fmt --auto-promote

check-format:
	dune build @fmt

release:
	dune build --profile release

test: default
	dune test

clean:
	dune clean

.PHONY: default format check-format release test clean
