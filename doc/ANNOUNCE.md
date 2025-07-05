<!-- {{{
This is a literate Haskell document! More on that towards the end. For now, I need to set up the language extensions, module header and 
imports so the code examples below compile.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module ANNOUNCE where

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

The length is just a unary-encoded natural number, lifted from the value level to the type level via [`DataKinds`][DataKinds].

  [datakinds]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html

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

zipList :: List n a -> List n b -> List n (a,b)
zipList = \cases
  Nil Nil -> Nil
  (Cons a as) (Cons b bs) -> Cons (a, b) (zipList as bs)
```

We can reuse the type-level naturals as an index for other parameterized types as well.

To jump right in and get very meta, consider the type `SNatural`.

```haskell
type SNatural :: Natural -> Type
data SNatural n where
  SZero :: SNatural Zero
  SSucc :: SNatural n -> SNatural (Succ n)
```

For each possible `n :: Natural` there's a type `SNatural n` with a single value (this is commonly called a singleton type).  Though a 
singleton doesn't sound very useful at first, it corresponds exactly to the length of the indexed list.

```haskell
lengthList :: List n a -> SNatural n
lengthList = \case
  Nil -> SZero
  Cons _ as -> SSucc (lengthList as)
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
lookupList :: FNatural n -> List n a -> a
lookupList = \cases
  FZero (Cons a _) -> a
  (FSucc fn) (Cons _ as) -> lookupList fn as
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
    least-significant-bit first order (LFO) or most-significant-bit first order (MFO).

    Consider the numbers 18, 19 and 20, with the binary expasions `0b10010`, `Ob10011`, and `0b10100` respectively. In LFO 
    they're `[O,I,O,O,I]`, `[I,I,O,O,I]`, and `[O,O,I,O,I]`. In MFO they're `[I,O,O,I,O]`, `[I,O,O,I,I]`, and `[I,O,I,O,O]`.

    To me, MFO seems like a clear winner in terms of readability; I'm used to reading binary strings like `Ob10011` left-to-right,
    having its encoding be `[I,O,O,I,I]` seems perfect.

    On the other hand, note how much of the list had to change between `[I,O,O,I,O]` and `[I,O,O,I,I]` when incrementing from one to the 
    other. We had to tear through the whole list to find the last element, then reconstruct it all the way back up. When pattern matching MFO, 
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
## Let's cut scope and simplify<!-- {{{ -->

Though `BNatural` doesn't look terribly complicated, any level of complexity we can remove in the index type will save loads 
of work down the line. In general, an indexed type will mimic the structure of the index. Note how the `List` type above 
mimics the structure of the `Natural` type, and the implementation of the `(++)` function mimics the `Plus` type family.

Instead, let's focus on creating a container type indexed by `Positive`. As a knock-on effect, this means that our type will 
always be non-empty, which can be a useful property. Later we can always create a wrapper type indexed by `BNatural` that is 
either empty or this `Positive`-indexed container type.
<!-- }}} -->
## Powers of two<!-- {{{ -->

Our index type, `Positive`, encodes places for each power of two, so intuition suggests a container type indexed by `Positive` 
might also have something to do with powers of two.

We could define various data types for storing each power of two:

```haskell
data Pow2_0 a = Pow2_0 a
data Pow2_1 a = Pow2_1 a a
data Pow2_2 a = Pow2_2 a a a a
data Pow2_3 a = Pow2_3 a a a a a a a a
```

But clearly that won't scale to storing arbitrarily large powers of two. However, looking again, we can see some sort of 
self-similarity possible here:

```haskell
data Pow2'2 a = Pow2'2 (Pow2_1 a) (Pow2_1 a)
data Pow2'3 a = Pow2'3 (Pow2'2 a) (Pow2'2 a)
data Pow2'4 a = Pow2'4 (Pow2'3 a) (Pow2'3 a)
```

We still wouldn't want to write all these types by hand, but we can extract the pattern to a GADT:

```haskell
type Pow2' :: Natural -> Type -> Type
data Pow2' n a where
  One' :: a -> Pow2' Zero a
  TwoTimes' :: Pow2' n a -> Pow2' n a -> Pow2' (Succ n) a
```

Oh man, unary numbers? I thought we were getting away from them. Well, at least they're counting `log(2^n)`.
I guess `Zero` and `Succ` are really just stand-ins for `ObI` and `(:. O)` here.

```haskell
type Pow2 :: Positive -> Type -> Type
data Pow2 n a where
  One :: a -> Pow2 ObI a
  TwoTimes :: Pow2 n a -> Pow2 n a -> Pow2 (n :. O) a
```

Edward Z. Yang describes a type isomorphic to `Pow2'` in his post _Two ways of representing binary trees_
([Yang, 2012][yang]):

  [yang]: http://blog.ezyang.com/2012/08/statically-checked-perfect-binary-trees/#nested-data-types

> One approach is to encode the size of the tree into the type, and then assert that the sizes of two trees are the same. This 
> is pretty easy to do with GADTs:
> 
>     data Z
>     data S n
>     
>     data L i a where
>         L :: a -> L Z a
>         N :: L i a -> L i a -> L (S i) a
> 
> By reusing the type variable i, the constructor of N ensures that we any two trees we combine must have the same size. 

He also pitches an alternative, and it's just dead sexy.

>     data B a = Two (B (a, a)) | One a
>         deriving Show
> 
> Notice how the recursive mention of B does not hold a, but (a, a): this is so-called “non-uniform” recursion. Every time we 
> apply a Two constructor, the size of our tuple doubles, until we top it off[.]
 
Instead of storing a balanced binary tree as collection of branching trees, we have a long spine of `Two` constructors leading 
to a tuple.

Since we want to keep track of the size, there's no reason we can't keep using an index with non-uniform recursion.

```haskell
type BalancedTree :: Positive -> Type -> Type
data BalancedTree n a where
  Leaves :: a -> BalancedTree ObI a
  Trunk :: BalancedTree n (a, a) -> BalancedTree (n :. O) a
```
<!-- }}} -->
## Can't see the tree for the forest<!-- {{{ -->

Now that we've got a couple ways of storing exactly `2^p` elements as binary trees, we can consider how to store an arbitrary 
number of elements. Binary encodings show how any arbitrary number can be represented as a sum of distinct powers of two, so 
that suggests our container type should be a collection of distinctly sized binary trees.

So to store seven elements, we could use a tree of size 4, a tree of size 2 and a tree of size 1:

```haskell
-- using Pow2
p4 :: Pow2 (ObI :. O :. O) Int
p4 = TwoTimes (TwoTimes (One 0) (One 1)) (TwoTimes (One 2) (One 3))

p2 :: Pow2 (ObI :. O) Int
p2 = TwoTimes (One 4) (One 5)

p1 :: Pow2 ObI Int
p1 = One 6

-- or BalancedTree
b4 :: BalancedTree (ObI :. O :. O) Int
b4 = Trunk $ Trunk $ Leaves ((0,1),(2,3))

b2 :: BalancedTree (ObI :. O) Int
b2 = Trunk $ Leaves (4,5)

b1 :: BalancedTree ObI Int
b1 = Leaves 6
```

But we can't just carry around these three trees in a list; they've got different types. And even if we could, we'd want to 
make sure they were all distinct types.

Here's where `BalancedTree`'s `Trunk` constructor merits a closer look. Consider these three `BalancedTree`s

```
  Trunk $ Trunk $ Trunk $ Trunk $ Trunk $ Trunk $ Trunk $ Leaves xs
  Trunk $ Trunk $ Trunk $ Trunk $ Trunk $ Leaves ys
  Trunk $ Trunk $ Trunk $ Leaves zs
                  ^^^^^
```

In all these cases, the `Trunk` constructors at the same depth all have the same type. For example, the careted third 
constructor is of type `Trunk :: BalancedTree n (((a,a),(a,a)),((a,a),(a,a))) -> BalancedTree (n :. O) ((a,a),(a,a))`. So if we 
altered `Branch` to also take at most one value of type `a`, we'd have a way of melding these distinct binary trees into one.

```haskell
type MeldedTree :: Positive -> Type -> Type
data MeldedTree n a where
  MLeaves :: a -> MeldedTree n a
  MTrunk :: MeldedTree n (a, a) -> Maybe a -> MeldedTree (n :. O) a
```

I put the recursive case on the left, which is a little odd in comparison with list, but it lines up with how in binary the 
left-most bits are the largest[^2].

[^2]: This also turns out to have some nice advantages when it comes to looking up elements by offset.

This still isn't quite right - we're not tracking the number of elements in the melded tree, we're tracking the size of the 
largest tree. But we can fix that with an indexed variant of `Maybe` - we use a `O` bit if there's no tree at this level and a 
`I` bit if there is.

Let's clean it up.
<!-- }}} -->
## The indexed pear tree<!-- {{{ -->

```haskell
type Tree :: Positive -> Type -> Type
data Tree n a where
  Canopy :: a -> Tree ObI a
  (:\) :: Tree n (Pair a) -> Opt b a -> Tree (n :. b) a

infixl 4 :\

type Pair :: Type -> Type
data Pair a = a :* a

infix 8 :*

type Opt :: Bit -> Type -> Type
data Opt b a where
  None :: Opt O a
  Some :: a -> Opt I a
```

Hey look at that, we finally got to the thing the title mentioned.

I call this a pear tree because it organizes a tree with pairs, pear is a homophone of pair in English, pears grow on trees, and 
I'm a sucker for a pun. It's possible someone already invented this[^*], but they probably didn't come up with nearly as good a 
name.[^3]

[^*]: Someone did! It's in Chris Okasaki's seminal thesis, [_Purely Functional Data Structures_][okasaki] as "Binary Random 
    Access Lists" in section 6.2.1.  Thanks to [/u/fire1299 on reddit][fire1299] for making the connection.

  [okasaki]: https://www.cs.cmu.edu/~rwh/students/okasaki.pdf
  [fire1299]: https://www.reddit.com/r/haskell/comments/1lrxa96/comment/n1i1mpz/

[^3]: I do feel for our english as a second language community, which is why rights are available for naming localization in 
    German, French, Swedish, and Glaswegian, among others.

The worth of a data structure, though, is all in what you can do with it. The `Functor`, `Foldable` and `Traversable` 
instances for `Tree n` are all easily derivable.

```haskell
deriving instance Functor (Tree n)
deriving instance Foldable (Tree n)
deriving instance Traversable (Tree n)

deriving instance Functor Pair
deriving instance Foldable Pair
deriving instance Traversable Pair

deriving instance Functor (Opt b)
deriving instance Foldable (Opt b)
deriving instance Traversable (Opt b)
```

We can measure trees using a singleton class for `Positive`s, just like we measured indexed lists using a singleton class for 
`Natural`.

```haskell
type SPositive :: Positive -> Type
data SPositive n where
  SObI :: SPositive ObI
  (:!) :: SPositive n -> SBit b -> SPositive (n :. b)

type SBit :: Bit -> Type
data SBit b where
  SO :: SBit O
  SI :: SBit I

treeSize :: Tree n a -> SPositive n
treeSize = \case
  Canopy _ -> SObI
  taa :\ oa -> treeSize taa :! optSize oa

optSize :: Opt b a -> SBit b
optSize = \case
  None -> SO
  Some _ -> SI
```

Similarly, we can use the fine set of naturals _less_ than the index `n` as offsets in `Tree n a` to look up values by 
position.

```haskell
-- the depth of the FObO or FPrefixO constructor determines which subtree the offset is in.
type FPositive :: Positive -> Type
data FPositive n where
  FObO :: FPositive ObI
  (:?) :: FPositive n -> Bit -> FPositive (n :. b)
  FPrefixO :: FPositive (n :. I)

lookupTree :: FPositive n -> Tree n a -> a
lookupTree = \cases
  FObO (Canopy a) -> a
  FPrefixO (_ :\ Some a) -> a
  (fn :? b) (taa :\ _) ->
    let (a0 :* a1) = lookupTree fn taa
    in case b of
        O -> a0
        I -> a1
```

Zipping two trees is fairly simple, we just walk the constructors together.

```haskell
zipWithTree :: (a -> b -> c) -> Tree n a -> Tree n b -> Tree n c
zipWithTree f = \cases
  (Canopy a) (Canopy b) -> Canopy (f a b)
  (taa :\ oa) (tbb :\ ob) -> zipWithTree (zipWithPair f) taa tbb :\ zipWithOpt f oa ob

zipWithPair :: (a -> b -> c) -> Pair a -> Pair b -> Pair c
zipWithPair f (a0 :* a1) (b0 :* b1) = f a0 b0 :* f a1 b1

zipWithOpt :: (a -> b -> c) -> Opt x a -> Opt x b -> Opt x c
zipWithOpt f = \cases
  None None -> None
  (Some a) (Some b) -> Some (f a b)
```

Just like the applicative for indexed lists is zipping, the applicative for `Tree`, `Pair` and `Opt` is also zipping.  
Although for `pure` it turns out we need the ability to infer _how big_ a tree should be created. And that's a job for a 
typeclass. Or two.

```haskell
class KnownPositive n where
  knownPositive :: SPositive n
instance KnownPositive ObI where
  knownPositive = SObI
instance (KnownPositive n, KnownBit b) => KnownPositive (n :. b) where
  knownPositive = knownPositive :! knownBit

class KnownBit b where
  knownBit :: SBit b
instance KnownBit O where
  knownBit = SO
instance KnownBit I where
  knownBit = SI

generateTree :: (FPositive n -> a) -> SPositive n -> Tree n a
generateTree f = \case
  SObI -> Canopy (f FObO)
  sn :! SO -> generateTree (\fn -> f (fn :? O) :* f (fn :? I)) sn :\ None
  sn :! SI -> generateTree (\fn -> f (fn :? O) :* f (fn :? I)) sn :\ Some (f FPrefixO)

instance KnownPositive n => Applicative (Tree n) where
  liftA2 = zipWithTree
  pure a = generateTree (const a) knownPositive
```
<!-- }}} -->
## Change is hard<!-- {{{ -->

What starts to get tricky with pear trees is operations that change the size of the tree. To make these work, we also need to 
define the change to the index parameter `n` at the type level.

For instance, for `push`ing an element onto a pear tree, we need a type-level `succ` for `Positive` numbers. And since this 
isn't Agda, we can't just reuse the value-level definition, we need to reimplement it as a type family.

```haskell
type PSucc :: Positive -> Positive
type family PSucc n where
  PSucc ObI = ObI :. O
  PSucc (n :. O) = n :. I
  PSucc (n :. I) = PSucc n :. O

push :: a -> Tree n a -> Tree (PSucc n) a
push a1 = \case
  Canopy a0 -> Canopy (a0 :* a1) :\ None
  taa :\ None -> taa :\ Some a1
  taa :\ Some a0 -> push (a0 :* a1) taa :\ None
```

We can also `pop` an element off a pear tree, though sadly we can't reuse `PSucc` for this as it's not injective. It _seems_ 
like it should be able to be one, but it's not, because even though you and I can see that `PSucc n` is always a `_ :. _`,
the typechecker can't be sure that it's not `ObI`. So we need to repeat ourselves and implement `PPred`.

```haskell
type PPred :: Positive -> Positive
type family PPred n where
  PPred (ObI :. O) = ObI
  PPred (n :. I) = n :. O
  PPred (bs :. b :. O) = PPred (bs :. b) :. I

pop :: Tree (bs :. b) a -> (Tree (PPred (bs :. b)) a, a)
pop = \case
  Canopy (a0 :* a1) :\ None -> (Canopy a0, a1)
  taa :\ Some a1 -> (taa :\ None, a1)
  taa@(_ :\ _) :\ None -> case pop taa of
    (taa', a0 :* a1) -> (taa' :\ Some a0, a1)
```

Combining two pear trees requires type-level addition for `Positive` numbers, which is a little more complex than it was for 
unary-encoded `Natural`s.

```haskell
type Add :: Positive -> Positive -> Positive
type Add i j = AddC i j O

fuse :: Tree n a -> Tree m a -> Tree (Add n m) a
fuse t0 t1 = fuseC t0 t1 None

type AddC :: Positive -> Positive -> Bit -> Positive
type family AddC i j b where
  AddC ObI ObI b = ObI :. b
  AddC (i :. O) ObI O = i :. I
  AddC (i :. O) ObI I = PSucc i :. O
  AddC (i :. I) ObI b = PSucc i :. b
  AddC ObI (j :. O) O = j :. I
  AddC ObI (j :. O) I = PSucc j :. O
  AddC ObI (j :. I) b = PSucc j :. b
  AddC (i :. O) (j :. O) b = Add i j :. b
  AddC (i :. O) (j :. I) O = Add i j :. I
  AddC (i :. O) (j :. I) I = AddC i j I :. O
  AddC (i :. I) (j :. O) O = Add i j :. I
  AddC (i :. I) (j :. O) I = AddC i j I :. O
  AddC (i :. I) (j :. I) b = AddC i j I :. b

fuseC :: Tree n a -> Tree m a -> Opt b a -> Tree (AddC n m b) a
fuseC = \cases
  (Canopy a0) (Canopy a1) oa -> Canopy (a0 :* a1) :\ oa
  (taa :\ None) (Canopy a1) None -> taa :\ Some a1
  (taa :\ None) (Canopy a1) (Some a2) -> push (a1 :* a2) taa :\ None
  (taa :\ Some a0) (Canopy a1) oa -> push (a0 :* a1) taa :\ oa
  (Canopy a0) (taa :\ None) None -> taa :\ Some a0
  (Canopy a0) (taa :\ None) (Some a2) -> push (a0 :* a2) taa :\ None
  (Canopy a0) (taa :\ Some a1) oa -> push (a1 :* a0) taa :\ oa
  (taa0 :\ None) (taa1 :\ None) oa -> fuse taa0 taa1 :\ oa
  (taa0 :\ None) (taa1 :\ Some a1) None -> fuse taa0 taa1 :\ Some a1
  (taa0 :\ None) (taa1 :\ Some a1) (Some a2) -> fuseC taa0 taa1 (Some (a1 :* a2)) :\ None
  (taa0 :\ Some a0) (taa1 :\ None) None -> fuse taa0 taa1 :\ Some a0
  (taa0 :\ Some a0) (taa1 :\ None) (Some a2) -> fuseC taa0 taa1 (Some (a0 :* a2)) :\ None
  (taa0 :\ Some a0) (taa1 :\ Some a1) oa -> fuseC taa0 taa1 (Some (a0 :* a1)) :\ oa
```

Unlike list conatenation, fusing two pear treas is an O(log n) operation. However, also unlike list concatenation, since `fuse 
t0 t1 = t` combines the two trees subtree-by-subtree, it's not true that all the elements of `t0` are before all the elements of 
`t1` in `t`. That is `toList t0 ++ toList t1 /= toList (fuse t0 t1)`.  In general `fuse` is not associative.

```
>>> fuse (Canopy ('E' :* 'F') :\ Some 'G') (Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ None :\ None)
Canopy (('a' :* 'b') :* ('c' :* 'd')) :\ Some ('E' :* 'F') :\ Some 'G'

>>> fuse (Canopy ('a' :* 'b') :\ Some 'c') (Canopy ('D' :* 'E') :\ Some 'F')
Canopy (('a' :* 'b') :* ('D' :* 'E')) :\ Some ('c' :* 'F') :\ None
```

This isn't a complete showstopper though, as even though the algorithm for computing offsets in the combined pear tree is more 
complicated than "length of the other list + old offset", it is still computable, and the offset of a value from before two 
trees were fused can be converted to the offset of the value after. Or vice versa.

```
>>> let t0 = Canopy ('a' :* 'b') :\ Some 'c'
>>> let t1 = Canopy ('D' :* 'E') :\ Some 'F'
>>> let t = fuse t0 t1
>>> t
Canopy (('a' :* 'b') :* ('D' :* 'E')) :\ Some ('c' :* 'F') :\ None

>>> let bij = offsetAdd (treeSize t0) (treeSize t1)
>>> let off = FObO :? O
>>> lookupTree off t0
'a'
>>> lookupTree (forwards bij $ FL off) t
'a'
>>> lookupTree off t1
'D'
>>> lookupTree (forwards bij $ FR off) t
'D'
```

(At ~150LOC, though, you'll forgive me for not inlining `offsetAdd` into this article)

`fuse` also has an inverse, `fizz` which runs the binary addition algorithm backwards to split one pear tree into two.

All of these functions are implemented in full in the `pear` package, [now on hackage](https://hackage.haskell.org/package/pear).
<!-- }}} -->
## Oooh, what else does it have?<!-- {{{ -->

That's going to be it for the first release. I've had versions of this code bouncing around my head for almost four years now 
as it moved from the front to the back burner numerous times, and while there's a lot of partial work I'm leaving on the 
cutting room floor, I think this is a good enough start. Revising this has kept me company as I drifted through a COVID 
infection and quarantine, but that's just about wrapped up.

There are a number of things I have planned though.

- `fizz` isn't super-ergonomic, as you need to specify the sizes of both the resulting trees. Next release will definitely 
  implement the binary subtraction algorithm for a more user-friendly `lsplitAt` and `rsplitAt`.
- `joinTree :: Tree n (Tree m a) -> Tree (n * m) a`
- `chunksOf :: SPositive d -> Tree n a -> (Tree (Quot n d) a, Tree (Rem n a) a)`

I think indexed Fibonacci trees would be amazing, but I think that's going to have to be someone else's project. Or at least a 
different package.

As we all know, a mathematician is a complicated device that transforms calories into theorems, so all thanks go to my wife 
Karlee, who kept me passing me plates of food while I was in quarantine in our guest room.

Feel free to make requests on [the issues page][issues] or join [the discussion on reddit][reddit].

  [reddit]: https://www.reddit.com/r/haskell/comments/1lrxa96/pear_trees_an_indexed_type_using_typelevel_binary/
  [issues]: https://github.com/rampion/pear/issues

<!-- }}} -->
# Literate Haskell<!-- {{{ -->

Because I don't trust myself not to have bugs in my examples, [this](?plain=1) is a literate haskell file. You can use 
[`markdown-unlit`][markdown-unlit] to compile all the code here thusly:

  [markdown-unlit]: https://github.com/sol/markdown-unlit

```
$ cabal build markdown-examples
...
[1 of 2] Compiling ANNOUNCE         ( test/ANNOUNCE.lhs, ...)
...
```
<!-- }}} -->
<!-- vim: set fo=walt wm=1 -->
