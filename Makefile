ghcid-library:
	ghcid \
		--command='cabal repl pear' \
		--no-height-limit \
		--test=":! cabal test pear-doctest" \
		--run=":! cabal haddock pear"

ghcid-markdown-examples:
	ghcid \
		--command='cabal repl markdown-examples' \
		--reload=doc
