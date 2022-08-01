The **pear** package provides a pattern-matchable binary encoding of
the natural numbers:

```haskell
data Binary = Ob | Binary :. Bit
data Bit = O | I
```

<!--
```haskell example
{- $setup
>>> :set -XBinaryLiterals
>>> import Pear

```
-->

```haskell example
>>> 0b10101 :: Binary
0b10101

```

<!--
```haskell example
-}
```
-->

Though this encoding can be used on its own, its primary purpose is as an
alternative to the unary encoding of the natural numbers often used at the type
level (`data Unary = Zero | Succ Unary`).

```haskell ignore
-- Singleton encoding
data SBinary (m :: Binary) where
  SOb :: SBinary 'Ob
  (:^) :: SBinary bs -> SBit b -> SBinary (bs ':. b)

data SBit (b :: Bit) where
  SO :: SBit 'O
  SI :: SBit 'I

-- The set of naturals less than a given natural, i.e. FBinary m ≈ { n | n < m }
data FBinary (m :: Binary) where
  FOb :: FBinary (bs ':. 'I)
  (:!) :: FBinary bs -> Bit -> FBinary (bs ': b)
```

As a motivating example, the package provides a `Vec` data type for collections
of known length encoded as a `Binary` value, which has values addressable in
O(log(n)) time, in contrast to the idiomatic `Unary` version which has `O(n)`
access time.

```haskell ignore
data Vec (m :: Binary) a where
  Nil :: Vec 'Ob a
  (:&) :: Vec bs (Two a) -> Opt b a -> Vec (bs ':. b) a

data Opt (b :: Bit) a where
  None :: Opt 'O a
  Some :: a -> Opt 'I a
```

TODO: add examples of use with Vec, e.g. !!, reverse, ++, combine, fromList, etc
