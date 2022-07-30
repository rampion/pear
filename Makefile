build-and-test-on-change:
	ghcid --lint --allow-eval --run --command "cabal repl base₂-library-test base₂-documentation-test" --restart base₂.cabal

test:
	cabal test --test-show-details=streaming --test-option=--color
