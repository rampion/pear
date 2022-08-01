spec:
	ghcid --target test:ℕ₂-spec --run

doctest:
	ghcid --target test:ℕ₂-doctest --run --reload=src/

test:
	cabal test --test-show-details=streaming --test-option=--color

.PHONY: test spec doctest
