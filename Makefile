spec:
	ghcid --target test:base₂-spec --run

doctest:
	ghcid --target test:base₂-doctest --run --reload=src/

test:
	cabal test --test-show-details=streaming --test-option=--color

.PHONY: test spec doctest
