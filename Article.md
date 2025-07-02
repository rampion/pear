<!-- {{{
This is a literate Haskell document! More on that towards the end. For now, I need to set up the language extensions, module header and 
imports so the code examples below compile.

```haskell
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Pear.Article where

import Data.Kind (Type)
import Prelude hiding (zip, length, head, tail, (++), lookup)
```
}}} -->
# Pear Trees: An indexed type using type-level binary numbers

> *I think that I shall never see a poem as lovely as a tree.*
> 
> &mdash; Joyce Kilmer, [*Trees*](https://www.poetryfoundation.org/poetrymagazine/poems/12744/trees)

## The indexed list type is cute<!-- {{{ -->

One thing I've seen off and on in haskell blogs is the indexed list type.  You've probably seen it around, but if you haven't, in general an 
**indexed type** ([Zenger, 1997][zenger]) is a type with a parameter indicating the size (or shape) of the data[^1].  The size parameter is 
called the index.  This is useful if you have some operations that are size-agnositic, and some that aren't.

  [zenger]: https://www.sciencedirect.com/science/article/pii/S0304397597000625

[^1]: All the indexed types I'm using here are going to be correct-by-construction, which isn't the only way to do them, nor necessarily the 
    most efficient. But I'm writing this, not you, and I think correct-by-construction is pretty.

The indexed list type is one of the simplest possible examples of an indexed data type. It augments the normal Haskell list type with the 
list's length.

```haskell
type List :: Natural -> Type -> Type
data List n a where
  Nil :: List Zero a
  Cons :: a -> List n a -> List (Succ n) a
```

The length is just a unary-encoded natural number.

```haskell
type Natural :: Type
data Natural = Zero | Succ Natural
```

We can use this index to have the compiler ensure:
- `head` or `tail` can only on be called on non-empty lists,
- `zip` only takes two lists of the same length.

```haskell
head :: List (Succ n) a -> a
head (Cons a _) = a

tail :: List (Succ n) a -> List n a
tail (Cons _ as) = as

zip :: List n a -> List n b -> List n (a,b)
zip = \cases
  Nil Nil -> Nil
  (Cons a as) (Cons b bs) -> Cons (a, b) (zip as bs)
```

We can reuse the type-level naturals as an index for other parameterized types as well.

To jump right and and get very meta, consider the type `SNatural`.

```haskell
type SNatural :: Natural -> Type
data SNatural n where
  SZero :: SNatural Zero
  SSucc :: SNatural n -> SNatural (Succ n)
```

For each possible `n :: Natural` there's a type `SNatural n` with a single value (this is commonly called a singleton type).  Though a 
singleton doesn't sound very useful at first, it corresponds exactly to the length of the indexed list.

```haskell
length :: List n a -> SNatural n
length = \case
  Nil -> SZero
  Cons _ as -> SSucc (length as)
```

Slightly more interestingly, we could create a type `FNatural n` representing the finite set of the natural numbers less than n.

```haskell
type FNatural :: Natural -> Type
data FNatural n where
  FZero :: FNatural (Succ n)
  FSucc :: FNatural n -> FNatural (Succ n)
```

Note that `FNatural Zero` is unpopulated and `FNatural (Succ Zero)` only has one value, `FZero`.

This finite subset exactly corresponds to the valid offsets in the linked list, making this perfect for looking up values by offset.

```haskell
lookup :: FNatural n -> List n a -> a
lookup = \cases
  FZero (Cons a _) -> a
  (FSucc fn) (Cons _ as) -> lookup fn as
```

Restricting our domain to known good offsets let us create a total `lookup` function.
  
We can concatenate indexed lists, we just have to teach the type-checker how addition works.

```haskell
(++) :: List n a -> List m a -> List (Plus n m) a
Cons a as ++ as' = Cons a (as ++ as')
Nil ++ as' = as'

type Plus :: Natural -> Natural -> Natural
type family Plus n m where
  Plus (Succ n) m = Succ (Plus n m)
  Plus Zero m = m
```

There's more that can be done with it, but those are the essentials. I'm sure there's a smorgasboard of packages on hackage providing indexed 
list types.  I've only ever used it on toy problems, so I've always just rolled it from scratch to get exactly the semantics I wanted, as it's 
not a lot of code.

And even when working on those toy problems, I've had a nagging suspicion. The indexed list type is cute, but shouldn't there be something 
cuter?

<!-- }}} -->
## Why am I using unary numbers when binary exists?<!-- {{{ -->

The thing about unary numbers (like `Natural` above) is that so many of their operations are asymptotically linear. O(`n`), to use the jargon.

Above, `head` and `tail` were both O(1), but `as ++ bs` is O(`|as|`) as it replicates unary addition behind the scenes. Checking two unary 
numbers for equality is also O(`n`), as is finding the minimum or the maximum. Multiplying unary numbers `n` and `m` is O(`n m`).

This usually isn't important, as far as I know.  I don't think there's any high-performance code out there using indexed lists concatenation.

But even so, using a O(`n`) addition algorithm on a computer when there exists a perfectly lovely O(`log n`) algorithm for addition of binary 
numbers offends me.

So what would a type indexed by binary numbers look like? First, we'll need bits.

<!-- }}} -->
## Gimme the bits<!-- {{{ -->

```haskell
type Bit :: Type
data Bit = O | I
  deriving (Show, Eq, Ord, Enum, Bounded)
```

We could encode all the natural numbers just as a list of bits, `[Bit]`, but I have two problems with that approach.

  - One is that it puts ease of reading at odds with ease of incrementing.

    We could use any arbitrary bit ordering when encoding binary numbers as `[Bit]`, but two of the most reasonable seem to be 
    least-signficant-bit first order (LFO) or most-significant-bit first order (MFO).

    Consider the number 18, 19 and 20, with the binary expasions `0b10010`, `Ob10011`, and `0b10100` respectively. In LFO they're 
    `[O,I,O,O,I]`, `[I,I,O,O,I]`, and `[O,O,I,O,I]`. In BFO they're `[I,O,O,I,O]`, `[I,O,O,I,I]`, and `[I,O,I,O,O]`.

    To me, BFO seems like a clear winner in terms of readability; I'm used to reading binary strings like `Ob10011` left-to-right,
    having its encoding be `[I,O,O,I,I]` seems perfect.

    On the other hand, note how much of the list had to change between `[I,O,O,I,O]` and `[I,O,O,I,I]` when incrementing from one to the 
    other. We had to tear through the whole list to find the last element, then reconstruct it all the way back up. When pattern matching BFO, 
    we'd have to know the length of the list in order to know whether the `I` in `I : _` was a 1, a 2, a 4, or a 4096.

    Using LFO, though, the leading bit position is always the 1-bit, so it's far more consistent. Incrementing from `O : [I,O,O,I]` to `I : 
    [I,O,O,I]` only updates the first cons cell of the list. A lot of recursive algorithms become easier when you don't need to know the 
    bit-list's length ahead of time, as you can keep track what power of two each bit position represents as you recurse through them.

    This is my minor problem with `[Bit]`.

  - The bigger issue is that it admit multiple encodings of zero. Just like in decimal, how `0`, `0.0`, `0.00`, and `0.000` are all synonyms 
    for zero, we can write zero's binary expansion as `0b0`, `0b00`, `0b0000`, or `0b00000000` (among others).

    Similarly, all of `[O]`, `[O,O]`, `[O,O,O,O]`, `[O,O,O,O,O,O,O,O]` are zero. We also have `[]`, which feels like it *should* be the 
    canonical one.

    But having multiple encodings for zero means we'll need to treat them all the same; which is going to be tricky when writing code at the 
    value level and harder when writing code at the top-level. We'll be working against the type-checker's instinct that `[]` and `[O]` are 
    different types.

    We could choose a canonical zero encoding, and pretend the others don't exist, but we'd still be having to comfort the compiler that the 
    non-canonical zero encodings aren't real and definitely aren't under the bed.

    It definitely would be veering away from being correct-by-construction, which just makes that much more mehh.
    
Using a custom data type for our string of bits gets around both of those issues.

```haskell
type BNatural :: Type
data BNatural = BZero | BPositive Positive

type Positive :: Type
data Positive = ObI | Positive :. Bit

infixl 4 :.
```

  - The `Positive` bit string is left-associative, so even though we humans can read `ObI :. O :. O :. I :. I` left-to-right as 19 in MFO,
    pattern-matching reads it right-to-left as `(((ObI :. O) :. O) :. I) :. I` in LFO.

  - `BZero` is the only encoding of zero.
<!-- }}} -->
## Let's cut scope and simplify
# Literate Haskell<!-- {{{ -->

Because I don't trust myself not to have bugs in my examples, this is a literate haskell file. You can use [`markdown-unlit`][markdown-unlit] 
to compile all the code here thusly:

  [markdown-unlit]: https://github.com/sol/markdown-unlit

```
# install markdown-unlit using the package manager of your choice, I'm using nix.
$ nix-shell -p haskellPackages.markdown-unlit
$ mkdir Pear
$ ln -s $PWD/Article.md Pear/Article.lhs
$ ghc -pgmL markdown-unlit Pear/Article.lhs
[1 of 1] Compiling Pear.Article     ( Pear/Article.lhs, Pear/Article.o )
```
<!-- }}} -->
<!-- vim: set fo=walt wm=1 -->  