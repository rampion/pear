  > *I think that I shall never see a poem as lovely as a tree.*
  > 
  > &mdash; Joyce Kilmer


It's been years since I first saw [this definition of a balanced binary tree on Edward Z. Yang's blog](http://blog.ezyang.com/2012/08/statically-checked-perfect-binary-trees/#nested-data-types), and I'm still struck by its beauty and elegance:

<!-- 
You've found the setup for my literate haskell!

```haskell example
{-# OPTIONS_GHC -Wall -Wextra -Werror -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Article where

import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (foldl')
``` -->

```haskell example
data BalancedTree a
  = Trunk (BalancedTree (a,a))
  | Canopy a

```
<!-- 
```haskell example
  deriving (Show, Eq, Functor, Foldable, Traversable)
``` -->

If you haven't seen non-uniform recursion before, this definition can seem a little odd. It almost looks more like a list than a tree.

Examining values reveals how `BalancedTree` works; it can only store exactly 2ᵏ elements, where `k` is the number of `Trunk` constructors.

|![visual representation of `Canopy a₀`](images/BalancedTreeSize1.png)|
|:-:|
|**`Canopy a₀`**|

|![visual representation of `Trunk (Canopy (a₀,a₁))`](images/BalancedTreeSize2.png)|
|:-:|
|**`Trunk (Canopy (a₀,a₁))`**|

|![visual representation of `Trunk (Trunk (Canopy ((a₀,a₁),(a₂,a₃))))`](images/BalancedTreeSize4.png)|
|:-:|
|**`Trunk (Trunk (Canopy ((a₀,a₁),(a₂,a₃))))`**|

|![visual representation of `Trunk (Trunk (Trunk (Canopy (((a₀,a₁),(a₂,a₃)),((a₄,a₅),(a₆,a₇))))))`](images/BalancedTreeSize8.png)|
|:-:|
|**`Trunk (Trunk (Trunk (Canopy (((a₀,a₁),(a₂,a₃)),((a₄,a₅),(a₆,a₇))))))`**|

Compare with the traditional binary tree[^1]:

```haskell example
data ArbitraryTree a
  = Branch (ArbitraryTree a) (ArbitraryTree a)
  | Leaf a
```
<!-- 
```haskell example
  deriving (Show, Eq, Functor, Foldable, Traversable)
``` -->

[^1]: One thing I think is cool is how close the two definitions are. 

      If we defined 

      ```haskell
      data Pair a = Pair a a
      ```

      Then we could make isomorphic definitions

      ```haskell example
      data BalancedTree' a  = Trunk'  (BalancedTree' (Pair a))   | Canopy' a
      data ArbitraryTree' a = Branch' (Pair (ArbitraryTree' a))  | Leaf' a
      ```

      With that, it's easy to see that the difference is really just the order of
      `Pair` and the tree type.

With optimal packing, it can also store 2ᵏ elements in a similar layout:

|![visual representation of `Leaf a₀`](images/ArbitraryTreeSize1.png)|
|:-:|
|**`Leaf a₀`**|

|![visual representation of `Branch (Leaf a₀) (Leaf a₁)`](images/ArbitraryTreeSize2.png)|
|:-:|
|**`Branch (Leaf a₀) (Leaf a₁)`**|

|![visual representation of `Branch (Branch (Leaf a₀) (Leaf a₁)) (Branch (Leaf a₂) (Leaf a₃))`](images/ArbitraryTreeSize4.png)|
|:-:|
|**`Branch (Branch (Leaf a₀) (Leaf a₁)) (Branch (Leaf a₂) (Leaf a₃))`**|

|![visual representation of `Branch (Branch (Branch (Leaf a₀) (Leaf a₁)) (Branch (Leaf a₂) (Leaf a₃))) (Branch (Branch (Leaf a₄) (Leaf a₅)) (Branch (Leaf a₆) (Leaf a₇)))`](images/ArbitraryTreeSize8.png)|
|:-:|
|**`Branch (Branch (Branch (Leaf a₀) (Leaf a₁)) (Branch (Leaf a₂) (Leaf a₃))) (Branch (Branch (Leaf a₄) (Leaf a₅)) (Branch (Leaf a₆) (Leaf a₇)))`**|

At first, the `Trunk` constructor can seem like a wasteful delay compared to
the immediate splitting `Branch` provides, but for a full balanced binary tree,
`BalancedTree` has slight advantage in terms of the overhead introduced by its
constructors, even compared with an optimally packed `ArbitraryTree`

```
number of elements    1     2     4     8     …     2ᵏ

                      number of constructors in tree

BalancedTree          1     3     6     11    …     2ᵏ + k
    Trunk             0     1     2     3     …     k
    Canopy            1     1     1     1     …     1
    (,)               0     1     3     7     …     2ᵏ - 1

ArbitraryTree         1     3     7     15    …     2ᵏ⁺¹ - 1
    Branch            0     1     3     7     …     2ᵏ - 1
    Leaf              1     2     4     8     …     2ᵏ
```

An optimally packed `ArbitraryTree` still has an advantage at indexing, since
the distance of each element from the tree's root is less

```
number of elements    1     2     4     8     …     2ᵏ

                      number of constructors between element and root

BalancedTree          1     3     5     7     …     2k + 1
    Trunk             0     1     2     3     …     k
    Canopy            1     1     1     1     …     1
    (,)               0     1     2     3     …     k

ArbitraryTree         1     2     3     4     …     k + 1
    Branch            0     1     2     3     …     k
    Leaf              1     1     1     1     …     1
```

Another advantage of `ArbitraryTree` over `BalancedTree` is that it can be
used to store an arbitrary (positive) number of elements, not just powers of
two.

|![visual representation of `Branch (Branch (Branch (Leaf a₀) (Leaf a₁)) (Leaf a₂)) (Branch (Leaf a₃) (Leaf a₄))`](images/ArbitraryTreeSize5.png)|
|:-:|
|**`Branch (Branch (Branch (Leaf a₀) (Leaf a₁)) (Leaf a₂)) (Branch (Leaf a₃) (Leaf a₄))`**|

But that flexibility comes with no guarantee of balance, allowing degenerate
cases that have list-like depth from the root.

|![visual representation of `Branch (Leaf a₀) (Branch (Leaf a₁) (Branch (Leaf a₂) (Branch (Leaf a₃) (Branch (Leaf a₄) (Branch (Leaf a₅) (Branch (Leaf a₆) (Leaf a₇)))))))`](images/LopsidedArbitraryTreeSize8.png)|
|:-:|
|**`Branch (Leaf a₀) (Branch (Leaf a₁) (Branch (Leaf a₂) (Branch (Leaf a₃) (Branch (Leaf a₄) (Branch (Leaf a₅) (Branch (Leaf a₆) (Leaf a₇)))))))`**|

[BETTER TRANSITION GOES HERE]

The rest of this article is an examination of a modified version of
`BalancedTree` that that, like `ArbitraryTree`, can store an arbitrary
(positive) number of elements while retaining the optimal packing quality of
the original.

```haskell example
data PearTree a
  = PearTree (a,a) :>- Maybe a
  | Top a

infixl 4 :>-
```

Here, `t :>- Nothing` plays the same role as `Trunk t` does for `BalancedTree`,
and `Top a` plays the same role as `Canopy a`.  The difference is `t :>- Just a`.[^2]

[^2]: I chose `(:>-) :: PearTree (a,a) -> Maybe a -> PearTree a` as my constructor name
      rather than something like `Fork :: PearTree (a,a) -> Maybe a -> PearTree a` or 
      having distinct constructors for the `Nothing` and `Just` cases because
      with an operator there's no need to wrap the contained `PearTree (a,a)`
      values in parentheses.

      ```haskell example
      fromNonEmpty :: NonEmpty a -> PearTree a
      fromNonEmpty (a :| as) = foldl' pushNonEmpty (Top a) as

      {- |
      >>> fromNonEmpty ('a' :| ['b'..'z']) :: PearTree Char
      Top
        ( ( ( ( 'a' , 'b' ) , ( 'c' , 'd' ) )
          , ( ( 'e' , 'f' ) , ( 'g' , 'h' ) )
          )
        , ( ( ( 'i' , 'j' ) , ( 'k' , 'l' ) )
          , ( ( 'm' , 'n' ) , ( 'o' , 'p' ) )
          )
        ) :>-
        Just
          ( ( ( 'q' , 'r' ) , ( 's' , 't' ) )
          , ( ( 'u' , 'v' ) , ( 'w' , 'x' ) )
          ) :>-
        Nothing :>-
        Just ( 'y' , 'z' ) :>-
        Nothing
      -}
      instance Show a => Show (PearTree a) where
        showsPrec p = showParen (p >= 4) . showsTree where
          showsTree :: forall a. Show a => PearTree a -> ShowS
          showsTree = \case
            Top a -> showString "Top " . showsPrec 10 a
            t :>- ma -> showsTree t . showString " :>- " . showsPrec 4 ma
      ```

Every positive integer has a unique binary encoding.

```
  base 10           base 2
        1              0b1
        2             0b10
        3             0b11
        4            0b100
        5            0b101
        …                …
        n             Σ_{k=0}^{⌊log₂ n⌋} bₖ · 2ᵏ, bₖ ∈ {0,1}
```

`PearTree` uses this fact to store `n` elements as a uniquely-determined
combination of balanced binary trees.

|![visual representation of `Top a₀`](images/PearTreeSize1.png)|
|:-:|
|**`Top a₀`**|

|![visual representation of `Top (a₀,a₁) :>- Nothing`](images/PearTreeSize2.png)|
|:-:|
|**`Top (a₀,a₁) :>- Nothing`**|

|![visual representation of `Top (a₀,a₁) :>- Just a₂`](images/PearTreeSize3.png)|
|:-:|
|**`Top (a₀,a₁) :>- Just a₂`**|

|![visual representation of `Top ((a₀,a₁),(a₂,a₃)) :>- Nothing :>- Nothing`](images/PearTreeSize4.png)|
|:-:|
|**`Top ((a₀,a₁),(a₂,a₃)) :>- Nothing :>- Nothing`**|


With this definition in hand, lets look at the definition of some common operations.

### `Functor`, `Foldable`, and `Traversable`

`PearTree` naturally has a `Functor` instance:

```haskell example
-- |
-- >>> fmap succ $ Top (('a','b'),('c','d')) :>- Nothing :>- Just 'e'
-- Top ( ( 'b' , 'c' ) , ( 'd' , 'e' ) ) :>- Nothing :>- Just 'f'
instance Functor PearTree where
  fmap f = \case
    Top a -> Top (f a)
    t :>- ma -> 
      let ff (a₀,a₁) = (f a₀, f a₁)
      in fmap ff t :>- fmap f ma
```

As is normal for recursive data structures, `PearTree`'s `fmap` is defined in
terms of itself.  However, as `PearTree`'s recursion is non-uniform, the
function `f` passed to `fmap` has the wrong type to pass to the recursive call
to `fmap` on the contained `PearTree (a,a)`.  Instead we must define a new
function, `ff`, that transforms each element of any pair of type `(a,a)`, and
pass that to the recursive call.[^3]

[^3]: Had we defined `PearTree` in terms of a custom pair functor, rather than the
      using the two-tuple, this new function would have just been defined using 
      that type's `fmap`:

      ```haskell example
      data PearTree' a = Top' a | PearTree' (Pair a) :>-: Maybe a

      data Pair a = Pair a a
        deriving Functor

      instance Functor PearTree' where
        fmap f = \case
          Top' a-> Top' (f a)
          t :>-: ma -> fmap (fmap f) t :>-: fmap f ma
      ```

`PearTree`'s `Foldable` is similar; again we must transform the function passed
to the recursive call to `foldMap` to account for the shift in type:

```haskell example
-- |
-- >>> import Data.Monoid (Sum(..))
-- >>> foldMap Sum $ Top (((1,2),(3,4)),((5,6),(7,8))) :>- Nothing :>- Just (9,10) :>- Just 11
-- Sum { getSum = 66 }
instance Foldable PearTree where
  foldMap f = \case
    Top a -> f a
    t :>- ma ->
      let ff (a₀, a₁) = f a₀ <> f a₁
      in foldMap ff t <> foldMap f ma
```

Ditto for `Traversable`:

```haskell example
-- |
-- >>> traverse (enumFromTo 0) $ Top (0,1) :>- Just 2
-- [ Top ( 0 , 0 ) :>- Just 0
-- , Top ( 0 , 0 ) :>- Just 1
-- , Top ( 0 , 0 ) :>- Just 2
-- , Top ( 0 , 1 ) :>- Just 0
-- , Top ( 0 , 1 ) :>- Just 1
-- , Top ( 0 , 1 ) :>- Just 2
-- ]
instance Traversable PearTree where
  traverse f = \case
    Top a -> Top <$> f a
    t :>- ma ->
      let ff (a₀, a₁) = (,) <$> f a₀ <*> f a₁
      in (:>-) <$> traverse ff t <*> traverse f ma
```

In fact, with `DeriveFuntor`, `DeriveFoldable`, and `DeriveTraversable`, ghc
can quite easily derive these instances itself.  However, it is informative to
see how the non-uniformity is handled in these simple cases before we get to
anything more complex.

### `push`/`pop`

For a `PearTree` of size `n`, push`ing an element on to the end takes time O(log₂ n).
It's just like incrementing a binary number; we add one to the lowest place,
and recursively carry into the next as long as is necessary

    input:      0b10111

    value:      0b10111
    place:            ^

    value:      0b10110
    place:           ^

    value:      0b10100
    place:          ^

    value:      0b10000
    place:         ^

    output:     0b11000

When we translate this metaphor to `PearTree`s:

  - `0b1` becomes `Top a`
  - `…0`   becomes `… :>- Nothing`
  - `…1`   becomes `… :>- Just a`

When we push an element `a₁` onto the end of a `PearTree a`, we can replace a
`Nothing` with a `Just a₁`, but a `Just a₀` is replaced by a `Nothing` and a
`(a₀,a₁)` value that needs to carried over by pushing it onto the linked
`PearTree (a,a)`.

    input:   Top (((('a','b'),('c','d')),(('e','f'),('g','h'))),((('i','j'),('k','l')),(('m','n'),('o','p'))))
                :>- Nothing
                :>- Just (('q','r'),('s','t'))
                :>- Just ('u','v')
                :>- Just 'w'

    tree:    Top (((('a'…'p')))) :>- Nothing :>- Just (('q'…'t')) :>- Just ('u','v') :>- Just 'w'
    push:                                                                                     'x'

    tree:    Top (((('a'…'p')))) :>- Nothing :>- Just (('q'…'t')) :>- Just ('u','v') :>- Nothing
    push:                                                                  ('w','x')

    tree:    Top (((('a'…'p')))) :>- Nothing :>- Just (('q'…'t')) :>- Nothing :>- Nothing
    push:                                             (('u'…'x'))

    tree:    Top (((('a'…'p')))) :>- Nothing :>- Nothing :>- Nothing :>- Nothing
    push:                            ((('q'…'x')))

    output:  Top (((('a'…'p')))) :>- Just ((('q'…'x'))) :>- Nothing :>- Nothing :>- Nothing
    

```haskell example
push :: Maybe (PearTree a) -> a -> PearTree a
push = maybe Top pushNonEmpty

pushNonEmpty :: PearTree x -> x -> PearTree x
pushNonEmpty = \case
  tx² :>- Just x₀ ->  \x₁ -> pushNonEmpty tx² (x₀,x₁) :>- Nothing
  tx² :>- Nothing ->  \x₁ -> tx² :>- Just x₁
  Top x₀ ->           \x₁ -> Top (x₀,x₁) :>- Nothing
```

Likewise, for `pop`, we can use the metaphor of decrementing a binary number.

    output:     0b11000

    value:      0b11000
    place:            ^

    value:      0b11000
    place:           ^

    value:      0b11000
    place:          ^

    value:      0b11000
    place:         ^

    value:      0b10000
    place:          ^

    value:      0b10100
    place:           ^

    value:      0b10110
    place:            ^

    output:     0b10111


Just as we "borrow" from the next higher place when we can't decrement the current one,
we `pop` from the linked `PearTree (a,a)` when the current tree ends in `… :>- Nothing`
and then decompose the returned `(a₀,a₁)` value to rebuild the updated tree
with `… :>- Just a₀` and pop the value `a₁`.

```haskell example
pop :: PearTree a -> (Maybe (PearTree a), a)
pop = \case
    Top a -> (Nothing, a)
    ta² :>- Just a -> (Just (ta² :>- Nothing), a)
    ta² :>- Nothing -> case popNonSingular ta² of
      (ta, a) -> (Just ta, a)
  where 
    popNonSingular :: PearTree (x,x) -> (PearTree x, x)
    popNonSingular (tx⁴ :>- Nothing) = case popNonSingular tx⁴ of
      (tx², (x₀,x₁)) -> (tx² :>- Just x₀, x₁)
    popNonSingular (tx⁴ :>- Just (x₀,x₁)) = (tx⁴ :>- Nothing :>- Just x₀, x₁)
    popNonSingular (Top (x₀,x₁)) = (Top x₀, x₁)
```

Although I prefer to use continuations so that the helper function is tail-call optimized:

```haskell example
pop' :: PearTree a -> (Maybe (PearTree a), a)
pop' = \case
    Top a -> (Nothing, a)
    ta² :>- Just a -> (Just (ta² :>- Nothing), a)
    ta² :>- Nothing -> popNonSingular ta² \ta a -> (Just ta, a)
  where 
    popNonSingular :: PearTree (x,x) -> (PearTree x -> x -> r) -> r
    popNonSingular (tx⁴ :>- Nothing) k = popNonSingular tx⁴ \tx² (x₀,x₁) -> k (tx² :>- Just x₀) x₁
    popNonSingular (tx⁴ :>- Just (x₀,x₁)) k = k (tx⁴ :>- Nothing :>- Just x₀) x₁
    popNonSingular (Top (x₀,x₁)) k = k (Top x₀) x₁
```

# indexing


top(x,k) = ⌊ x / 2ᵏ ⌋
btm(x,k) = x mod 2ᵏ 
bit(x,k) = top(x,k) mod 2



XX To look up elements by their index in a `PearTree`, we first have to establish
XX what the order of elements in a `PearTree` is.  There's `n!` possible orders
XX for a collection of `n` elements, but there's two most obvious candidates:
XX 
XX   - order the elements in the same order that they would have needed to be `push`ed in;
XX     e.g. `Top ((0,1),(2,3)) :>- Just (4,5) :>- Just 6`
XX     
XX   - order the elements in the opposite order that they would have been `push`ed in; e.g.
XX     `Top ((6,5),(4,3)

Given a tree of size

  m = m₀·2⁰ + m₁·2¹ + … + m_{n-1}·2^{n-1}

And an index

  j = j₀·2⁰ + j₁·2¹ + … + j_{k-1}·2^{k-1}

We can find the element with index j in O(log₂ m) = O(n) steps. 

By step t, we've examined the `t` least significant bits of `j`


```haskell example
(#) :: forall a. PearTree a -> Int -> Maybe a
(#) = loop id Nothing where
  loop :: forall aⁿ. (aⁿ -> a) -> Maybe a -> PearTree aⁿ -> Int -> Maybe a
  loop from next = \case
    ta²ⁿ :>- maⁿ -> \ix -> 
      let ma = from <$> maⁿ
          (q,r) = ix `quotRem` 2
      in 
      if r == 0 
        then loop (from . fst) (ma <|> next) ta²ⁿ q
        else loop (from . snd) (ma *> next) ta²ⁿ q
    Top aⁿ -> \case
      0 -> Just (from aⁿ)
      1 -> next
      _ -> Nothing
```



Suppose we have a `PearTree` with `n` elements, where `n = b_{w-1} · 2^{w-1} +
… + b_1 · 2^{1} + b_0 · 2^{0}`; as we work towards the `Top` of the `PearTree`
we examine our index, so by the time examine our `k`'th constructor, we've
examined the `k` least significant bits of the index.

[BLAH BLAH BLAH - look at the skyline of the size]

We can even create a pseudooptic:

```haskell example
newtype a ∊ t = Selected { withSelected :: forall f. Functor f => (a -> f a) -> f t }

-- |
-- >>> import Data.Functor.Identity (Identity(..))
-- >>> tree = Top (('a','b'),('c','d')) :>- Nothing :>- Just 'e'
-- >>> tree
-- Top ( ( 'a' , 'b' ) , ( 'c' , 'd' ) ) :>- Nothing :>- Just 'e'
-- >>> at 2 tree <&> runIdentity . \selection -> withSelected selection (Identity . const '_')
-- Just 
--   (Top ( ( 'a' , 'b' ) , ( '_' , 'd' ) ) :>- Nothing :>- Just 'e')
at :: forall a. Int -> PearTree a -> Maybe (a ∊ PearTree a)
at = loop id id Nothing where
  loop :: forall aⁿ. 
    (forall f. Functor f => (a -> f a) -> aⁿ -> f aⁿ) -> 
    (PearTree aⁿ -> PearTree a) -> Maybe (a ∊ PearTree a) -> 
    Int -> PearTree aⁿ -> Maybe (a ∊ PearTree a)
  loop lens wrap next ix = \case
    ta²ⁿ :>- maⁿ -> 
      let here :: Maybe (a ∊ PearTree a)
          here = maⁿ <&> \aⁿ -> Selected \f -> wrap . (ta²ⁿ :>-) . Just <$> lens f aⁿ
          (q,r) = ix `quotRem` 2
      in 
      if r == 0
        then loop (_1 . lens) (wrap . (:>- maⁿ)) (here <|> next) q ta²ⁿ
        else loop (_2 . lens) (wrap . (:>- maⁿ)) (here *> next) q ta²ⁿ
    Top aⁿ -> case ix of
      0 -> Just do Selected \f -> wrap . Top <$> lens f aⁿ
      1 -> next
      _ -> Nothing

  _1 :: forall x y e f. Functor f => (x -> f y) -> (x,e) -> f (y,e)
  _1 f (x,e) = f x <&> \y -> (y,e)

  _2 :: forall x y e f. Functor f => (x -> f y) -> (e,x) -> f (e,y)
  _2 f (e,x) = f x <&> \y -> (e,y)
```

# `fromList` / `toList`


# `Semigroup`

----

We can also create a size-indexed variant of `PearTree`; similar to the size-indexed variant of `[]`, with the advantage that many operations drop from linear to log time.

  [note]: *
    Or you could just use a newtype wrapped vector and integers for O(1) indexing if you wanted real speed and efficiency. But this is more fun, don't you think?
    * 
