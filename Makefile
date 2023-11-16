ghcid-spec:
	ghcid --target pear --run=":! ghcid --target pear-spec --run"
.PHONY: ghcid

ghcid-doctest:
	ghcid --target pear --run=":! ghcid --target pear-doctest --run"
.PHONY: ghcid

ghcid-haddock:
	ghcid --target pear --test=":! cabal haddock"
.PHONY: ghcid

# preview-article:
# 	echo doc/Article.md | entr make doc/Article.html
# .PHONY: preview-article
# 
# doctest-article:
# 	cabal exec cabal test doctest-article
# .PHONY: doctest-article
# 
# ghcid-article:
# 	ghcid --test=":! make doctest-article"
# .PHONY: ghcid-article
# 
# doc/%.html: doc/%.md
# 	pandoc --standalone $(addprefix --css ../,$(wildcard doc/css/*)) --from gfm --to html --metadata=title:$* $< -o $@

# spec:
# 	ghcid --target test:pear-spec --run
# 
# doctest:
# 	ghcid --target test:pear-doctest --run --reload=src/
# 
# test:
# 	cabal test --test-show-details=streaming --test-option=--color
# 
# haddock:
# 	ghcid --target pear --test=':! cabal haddock'
# 
# .PHONY: test spec doctest haddock article article-html article-doctest
