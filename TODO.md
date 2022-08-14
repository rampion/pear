- [ ] document FILES.md
  - [ ] write documentation as we add each file

- [X] rename package?


# Pear.Finite
# Pear.Bit.Finite
# Pear.Binary.Finite
# Pear.Indexed
# Pear.Indexed.Functor
# Pear.Indexed.Applicative
# Pear.Indexed.Monad
# Pear.Indexed.Foldable
# Pear.Indexed.Traversable

# Pear.Singleton.Applicative
- maybe rename? also needs Via, Finite, ...

# Pear.Two
- I\* instances?
# Pear.Opt
- I\* instances
# Pear.Vec
- [ ] clean up combine and (++)
- ? express (++) using cons?
    2^k + 2^{m+k} + ... = 2^k (1 + 2^m) + ...
                                ^- cons

# Pear
- [ ] re-export everything

---

# Notes from gmail:

## 10/25 7:11am

```haskell
withList :: [a] -> (forall bs. Vec ('Binary bs) a -> r) -> r
pair :: a -> [a] -> (Vec ('Binary 'ObI) a -> r) -> (forall b. Two a -> [Two a] -> Opt b a -> r) -> r
```

## 10/25 1:36pm

```haskell
withList :: (forall bs. Vec ('Binary bs) a -> r) -> [a] -> r
pair :: (Vec ('Binary 'ObI) a -> r) -> (forall b. Two a -> [Two a] -> Opt b a -> r) -> a -> [a] -> r
```

## 10/26 7:37am

version control
version control
version control

ideas to speed up compilation:
- fragment into individual functions e.g. combineCarryBranchSomeLeafNone?
- keep things in scope less with finCarry :: (Fin' m -> r) -> (Fin' n -> r) -> ((b ~ 'I) => r) -> r and try helper functions again?

## 11/1 3:30am

FILES.md
- doc/posts/announcement.md

```haskell
data Binary = Ob | Binary :. Bit

instance Num Binary
instance Bits Binary
fromNatural :: Natural-> Binary
toNatural :: Binary -> Natural

data SBinary m where
  SOb :: SBinary Ob
  (:|) :: SBinary bs -> SBit b -> SBinary (bs :. b)

data SomeBinary where
  SomeBinary :: SBinary m -> SomeBinary

type Zero :: Binary -> Bool
type Canonical :: Binary -> Bool
type family Canonical m where
   Canonical Ob = True
   Canonical (bs :. I) = Canonical bs
   Canonical (bs :. O) = Canonical bs `And` Not (Zero bs))
-- might need CanonicalNonZero helper

type WeaklyKnownBinary :: Binary -> Constraint
type KnownBinary :: Binary -> Constant
```

https://hackage.haskell.org/package/indexed-traversable
 
https://hackage.haskell.org/package/reform-0.2.7.4/docs/Control-Applicative-Indexed.html

no scoped packages :(

## 11/1 7:53am

use a cursor for append

```haskell
cursor :: KnownBinary m => Cursor Ob m a
appendTree :: Vec j a -> Cursor k n a -> Cursor (j + k) n a
toVec :: Cursor m m a -> Vec m a
```

## 11/8 5:05am

quasiquoters

```haskell
[vec|1,2,3,4,5]
Nil :& Some [bbt|1,2,3,4] :& None :& Some 5
```

use implicit params to choose show, default to vec quasiquoter to avoid lisping
