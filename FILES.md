# FILES.md
A brief description of each file in this repository.

# README.md
An user's introduction to the **pear** package.

# pear.cabal
A description of the **pear** package, used by `cabal`.

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

# src/Pear/Bit/Singleton.hs
A singleton type for Bit

# src/Pear/Binary/Singleton.hs
A singleton type for Binary

# src/Pear/Finite.hs
A class for associating a value of a type with a subset of that type.

# src/Pear/Bit/Finite.hs
A finite type for Bit.

# src/Pear/Binary/Finite.hs
A finite type for Binary.

# src/Pear/Indexed.hs
Indexed variants of `Functor`, `Applicative`, `Monad`, `Foldable` and `Traversable`.

# src/Pear/Singleton/Applicative.hs
Variant of `IApplicative` that takes an explicit shape parameter.

# src/Pear/Via/Deindexed.hs
Helper newtype for deriving non-indexed typeclasses from their indexed variants.

# src/Pear/Via/Elem.hs
Helper newtype for deriving instances for one typeclass using instances of another

# src/Pear/Opt.hs
Counted variant of `Maybe`

# src/Pear/Two.hs
A strict pair of values of the same type.

# src/Pear/Vec.hs
A length-indexed collection type

# src/Pear/Vec/Combine.hs
An algorithm for combining two `Vec`s

# src/Pear/README.lhs

# cabal.project
# .ghcid
# .vimrc
# .gitignore

# Setup.hs
A program to perform setup tasks when building the package.

# CHANGELOG.md
A list of changes introduced at each version of the package.

# src/Pear/Example.hs

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
