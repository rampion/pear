# FILES.md
A brief description of each file in this repository.

# README.md
An user's introduction to the **base₂** package.

# base₂.cabal
A description of the **base₂** package, used by `cabal`.

# src/Pear.hs
Umbrella module reexporting the contents of the modules in the `src/Pear` directory.

# src/Pear/Bit.hs
A custom type for representing a single bit.

# src/Pear/Binary.hs
A custom type for representing a natural number as a sequence of bits.

# src/Pear/Function.hs
A custom type for bundling a function along with its inverse image.

# src/Pear/Singleton.hs
A class and family for mapping between values and singleton types.

# src/Pear/Singleton/Known.hs
A constraint for reifying a singleton value for a type.

# src/Pear/Singleton/Applicative.hs

# src/Pear/Bit/Singleton.hs
A singleton type for Bit

# src/Pear/Binary/Singleton.hs
A singleton type for Binary

# src/Pear/Binary/Finite.hs
# src/Pear/Bit/Finite.hs
# src/Pear/Finite.hs

# src/Pear/Example.hs
# src/Pear/Indexed.hs
# src/Pear/Opt.hs
# src/Pear/README.lhs

# src/Pear/Two.hs
# src/Pear/Vec.hs
# src/Pear/Via/Deindexed.hs
# src/Pear/Via/Elem.hs

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
# test/Pear/Binary/Test.hs
# test/Pear/Bit/Test.hs
# test/Pear/Test.hs
