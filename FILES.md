# FILES.md
A brief description of each file in this repository.

# README.md
An user's introduction to the **binary-naturals** package.

# src/Data/Natural/Binary.hs
Umbrella module reexporting the contents of the modules in the `src/Data/Natural/Binary/` directory.

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

# binary-naturals.cabal
A description of the **binary-naturals** package, used by `cabal`.

# Setup.hs
A program to perform setup tasks when building the package.

# CHANGELOG.md
A list of changes introduced at each version of the package.
