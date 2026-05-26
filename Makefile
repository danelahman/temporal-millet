default: format
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

vscode-extension:
	cd editors/vscode && \
	  rm -f vscode-temporal-millet-*.vsix && \
	  npx --yes @vscode/vsce package && \
	  code --install-extension vscode-temporal-millet-*.vsix --force

.PHONY: default format check-format release test clean vscode-extension
