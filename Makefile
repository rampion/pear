spec:
	ghcid --target test:pear-spec --run

doctest:
	ghcid --target test:pear-doctest --run --reload=src/

test:
	cabal test --test-show-details=streaming --test-option=--color

.PHONY: test spec doctest
