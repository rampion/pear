build-and-test-on-change:
	ghcid --lint --allow-eval --run --command "cabal repl base₂-library-test base₂-documentation-test" --restart base₂.cabal
