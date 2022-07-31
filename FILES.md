# FILES.md
A brief description of each file in this repository.

# README.md
An user's introduction to the **base₂** package.

# base₂.cabal
A description of the **base₂** package, used by `cabal`.

# src/Base₂.hs
Umbrella module reexporting the contents of the modules in the `src/Base₂` directory.

# src/Base₂/Binary.hs
# src/Base₂/Binary/Finite.hs
# src/Base₂/Binary/Singleton.hs
# src/Base₂/Bit.hs
# src/Base₂/Bit/Finite.hs
# src/Base₂/Bit/Singleton.hs
# src/Base₂/Example.hs
# src/Base₂/Finite.hs
# src/Base₂/Function.hs
# src/Base₂/Indexed.hs
# src/Base₂/Opt.hs
# src/Base₂/README.lhs
# src/Base₂/Singleton.hs
# src/Base₂/Singleton/Applicative.hs
# src/Base₂/Singleton/Known.hs
# src/Base₂/Two.hs
# src/Base₂/Vec.hs
# src/Base₂/Via/Deindexed.hs
# src/Base₂/Via/Elem.hs

# cabal.project
# .ghcid
# .vimrc
# .gitignore

# src/Data/Natural/Binary/Type.hs
Definitions of the core types, `Binary` and `Bit`, and their instances.

# src/Data/Natural/Binary/Finite.hs
Definitions of the finite types, `FBinary` and `FBit`, and their instances.

# src/Data/Natural/Binary/Singleton.hs
Definitions of the singleton types, `SBinary` and `SBit`, and their instances.

# src/Data/Natural/Binary/Two.hs
Definition of `Two a`, a strict version of `(a,a)`, and its instances.

# src/Data/Natural/Binary/Opt.hs
Definition of `Opt :: Bit -> Type -> Type`, a dependently-typed variant of `Maybe`, and its instances.

# src/Data/Natural/Binary/Vec.hs
Definition of `Vec :: Binary -> Type -> Type`, a dependently-typed variant of `[]`, and its instances.

# src/Data/Natural/Binary/Combine.hs
Definition of `combine`, an `O(log(m + n))` algorithm for shuffling a `Vec m` and a `Vec n` into a `Vec (m + n)`.

# src/Data/Natural/Binary/Append.hs
Definition of `(++)`, an `O(m + n)` algorithm for concatenating a `Vec m` and a `Vec n` into a `Vec (m + n)`.

# test/Main.hs

# Setup.hs
A program to perform setup tasks when building the package.

# CHANGELOG.md
A list of changes introduced at each version of the package.

# ANNOUNCE.md
# Makefile
# TODO.md
# doc/Article.html
# doc/Article.lhs
# doc/Article.md
# doc/Article/Marge.png
# doc/Article/Marge.xcf
# doc/Article/balanced-trees.dot
# doc/Article/balanced-trees.png
# doc/Article/unbalanced-trees.dot
# doc/Article/unbalanced-trees.png
# doc/Article/uncounted-forest.dot
# doc/Article/uncounted-forest.png
# test/Base₂/Binary/Test.hs
# test/Base₂/Bit/Test.hs
# test/Base₂/Test.hs
