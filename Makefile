build-and-test-on-change:
	ghcid --lint --allow-eval --run --command "cabal repl base₂-library-test base₂-documentation-test" --restart base₂.cabal

dev:
	ghcid --target test:base₂-spec --run

test:
	cabal test --test-show-details=streaming --test-option=--color

.PHONY: test
