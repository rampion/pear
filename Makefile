spec:
	ghcid --target test:pear-spec --run

doctest:
	ghcid --target test:pear-doctest --run --reload=src/

test:
	cabal test --test-show-details=streaming --test-option=--color

haddock:
	ghcid --target pear --test=':! cabal haddock'

.PHONY: test spec doctest haddock
