> *I think that I shall never see a poem as lovely as a tree.*
> 
> &mdash; Joyce Kilmer

It's been years since I first saw [this definition of a balanced binary tree on Edward Z. Yang's blog](http://blog.ezyang.com/2012/08/statically-checked-perfect-binary-trees/#nested-data-types), and I'm still struck by its beauty and elegance:

```haskell
data BalancedTree a
  = Trunk (BalancedTree (a,a))
  | Canopy a
```

If you haven't seen non-uniform recursion before, this definition can seem a little odd. It almost looks more like a list than a tree.

Examining values reveals how `BalancedTree` works; it can only store exactly 2^k elements, where `k` is the number of `Trunk` constructors.

![visual representation of `Canopy a₀`](images/BalancedTreeSize1.png)
![visual representation of `Trunk (Canopy (a₀,a₁))`](images/BalancedTreeSize2.png)
![visual representation of `Trunk (Trunk (Canopy ((a₀,a₁),(a₂,a₃))))`](images/BalancedTreeSize4.png)
![visual representation of `Trunk (Trunk (Trunk (Canopy (((a₀,a₁),(a₂,a₃)),((a₄,a₅),(a₆,a₇))))))`](images/BalancedTreeSize8.png)

Compare with the traditional binary tree:

```haskell
data ArbitraryTree a
  = Branch (ArbitraryTree a) (ArbitraryTree a)
  | Leaf a
```

  [note]: *
    One thing I think is cool is how close the two definitions are. 

    If we defined 

    ```haskell
    type Pair a = (a,a)
    ```

    Then we could make isomorphic definitions

    ```haskell
    data BalancedTree' a  = Trunk'  (BalancedTree' (Pair a))   | Canopy' a
    data ArbitraryTree' a = Branch' (Pair (ArbitraryTree' a))  | Leaf' a
    ```

    With that, it's easy to see that the difference is really just the order of
    `Pair` and the tree type.

With optimal packing, it can also store 2^k elements in a similar layout:

![visual representation of `Leaf a₀`](images/ArbitraryTreeSize1.png)
![visual representation of `Branch (Leaf a₀) (Leaf a₁)`](images/ArbitraryTreeSize2.png)
![visual representation of `Branch (Branch (Leaf a₀) (Leaf a₁)) (Branch (Leaf a₂,a₃))))`](images/ArbitraryTreeSize4.png)
![visual representation of `Branch (Branch (Branch (Leaf a₀) (Leaf a₁)) (Branch (Leaf a₂) (Leaf a₃))) (Branch (Branch (Leaf a₄) (Leaf a₅)) (Branch (Leaf a₆) (Leaf a₇)))`](images/ArbitraryTreeSize8.png)

At first, the `Trunk` constructor can seem like a wasteful delay compared to
the immediate splitting `Branch` provides, but for a full balanced binary tree,
`BalancedTree` has slight advantage in terms of the overhead introduced by its
constructors, even compared with an optimally packed `ArbitraryTree`

```
number of elements    1     2     4     8     …     2^k

                      number of constructors in tree

BalancedTree          1     3     6     11    …     2^k + k
    Trunk             0     1     2     3     …     k
    Canopy            1     1     1     1     …     1
    (,)               0     1     3     7     …     2^k - 1

ArbitraryTree         1     3     7     15    …     2^{k+1} - 1
    Branch            0     1     3     7     …     2^k - 1
    Leaf              1     2     4     8     …     2^k
```

An optimally packed `ArbitraryTree` still has an advantage at indexing, since
the distance of each element from the tree's root is less

```
number of elements    1     2     4     8     …     2^k

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

[INSERT PICTURE]

But that flexibility comes with no guarantee of balance, allowing degenerate
cases that have list-like depth from the root.

[INSERT PICTURE]

The rest of this article is an examination of a modified version of
`BalancedTree` that that, like `ArbitraryTree`, can store an arbitrary
(positive) number of elements while retaining the optimal packing quality of
the original.

```haskell
data PearTree a
  = PearTree (a,a) :>- Maybe a
  | Top a

infixl 4 :>-
```

Here, `t :>- Nothing` plays the same role as `Trunk t` does for `BalancedTree`,
and `Top a` plays the same role as `Canopy a`.  The difference is `t :>- Just a`.

  [note]: *
    I chose `(:>-) :: PearTree (a,a) -> Maybe a -> PearTree a` as my constructor name
    rather than something like `Fork :: PearTree (a,a) -> Maybe a -> PearTree a` or 
    having distinct constructors for the `Nothing` and `Just` cases because I
    thought it made the `Show` implementation prettier.

    ```
    $> fromList ['a'..'z'] :: PearTree Char
    Top (((('a','b'),('c','d')),(('e','f'),('g','h'))),((('i','j'),('k','l')),(('m','n'),('o','p'))))
      :>- Just ((('q','r'),('s','t')),(('u','v'),('w','x'))) 
      :>- Nothing 
      :>- Just ('y','z') 
      :>- Nothing
    ```

Every positive integer has a unique binary encoding.

```
  base 10           base 2
        1              0b1
        2             0b10
        3             0b13
        4            0b100
        5            0b101
        …                …
        n             Σ_{k=0}^{⌊log₂ n⌋} b_k · 2^{k}, b_k ∈ {0,1}
```

`PearTree` uses this fact to store `n` elements as a uniquely-determined
combination of balanced binary trees.

[INSERT PICTURE]

With this definition in hand, lets look at the definition of some common operations.

# `push`/`pop`

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

    input:   Top (((('a','b'),('c','d')),(('e','f'),('g','h'))),((('i','j'),('k','l')),(('m','n'),('o','p')))) :>- Nothing :>- Just (('q','r'),('s','t')) :>- Just ('u','v') :>- Just 'w'

    tree:    Top (((('a'…'p')))) :>- Nothing :>- Just (('q'…'t')) :>- Just ('u','v') :>- Just 'w'
    push:                                                                                     'x'

    tree:    Top (((('a'…'p')))) :>- Nothing :>- Just (('q'…'t')) :>- Just ('u','v') :>- Nothing
    push:                                                                  ('w','x')

    tree:    Top (((('a'…'p')))) :>- Nothing :>- Just (('q'…'t')) :>- Nothing :>- Nothing
    push:                                             (('u'…'x'))

    tree:    Top (((('a'…'p')))) :>- Nothing :>- Nothing :>- Nothing :>- Nothing
    push:                            ((('q'…'x')))

    output:  Top (((('a'…'p')))) :>- Just ((('q'…'x'))) :>- Nothing :>- Nothing :>- Nothing
    

```haskell
push :: Maybe (PearTree a) -> a -> PearTree a
push = maybe Top pushNonEmpty where
  pushNonEmpty :: PearTree x -> x -> PearTree x
  pushNonEmpty (tx² :>- Just x₀) x₁ = pushNonEmpty tx² (x₀,x₁) :>- Nothing
  pushNonEmpty (tx² :>- Nothing) x₁ = tx² :>- Just x₁
  pushNonEmpty (Top x₀) x₁ = Top (x₀,x₁) :>- Nothing
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

```haskell
pop :: PearTree a -> (Maybe (PearTree a), a)
pop = \case
    Top a -> (Nothing, a)
    ta² :>- Just a -> (Just (ta² :>- Nothing), a)
    ta² :>- Nothing -> case popNonSingular ta² of
      (ta, a) -> (Just ta, a)
  where 
    popNonSingular :: PearTree (x,x) -> (PearTree x, x)
    popNonSingular (tx⁴ :>- Nothing) k = case popNonSingular tx⁴ of
      (tx², (x₀,x₁)) -> (tx² :>- Just x₀, x₁)
    popNonSingular (tx⁴ :>- Just (x₀,x₁)) = (tx⁴ :>- Nothing :>- Just x₀, x₁)
    popNonSingular (Top (x₀,x₁)) k = (Top x₀, x₁)
```

Although I prefer to use continuations so that the helper function is tail-call optimized:

```haskell
pop :: PearTree a -> (Maybe (PearTree a), a)
pop = \case
    Top a -> (Nothing, a)
    ta² :>- Just a -> (Just (ta² :>- Nothing), a)
    ta² :>- Nothing -> popNonSingular ta² \ta a -> (Just ta, a)
  where 
    popNonSingular :: PearTree (x,x) -> (PearTree x -> x -> r) -> r
    popNonSingular (tx⁴ :>- Nothing) k = popNonSingular tx⁴ \tx² (x₀,x₁) -> k (tx² :>- Just x₀) x₁
    popNonSingular (tx⁴ :>- Just (x₀,x₁)) = k (tx⁴ :>- Nothing :>- Just x₀) x₁
    popNonSingular (Top (x₀,x₁)) k = k (Top x₀) x₁
```

# indexing


top(x,k) = ⌊ x / 2^k ⌋
btm(x,k) = x mod 2^k 
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

  m = m_0·2^0 + m_1·2^1 + … + m_{n-1}·2^{n-1}

And an index

  j = j_0·2^0 + j_1·2^1 + … + j_{k-1}·2^{k-1}

We can find the element with index j in O(log₂ m) = O(n) steps. 

By step t, we've examined the `t` least significant bits of `j`


```haskell
(#) :: forall a. PearTree a -> Int -> Maybe a
(#) = loop id Nothing where
  loop :: forall x. (x -> a) -> Maybe a -> PearTree x -> Int -> a
  loop from next = \case
    t :>- mb -> \ix -> 
      let ma = from <$> mb
          (q,r) = ix `quotRem` 2
      in 
      case r of
        0 -> loop (from . fst) (ma <|> next) t
        ~1 -> loop (from . snd) (ma *> next) t
    Top b -> \case
      0 -> Just (f b)        
      1 -> next
      _ -> Nothing
```



Suppose we have a `PearTree` with `n` elements, where `n = b_{w-1} · 2^{w-1} +
… + b_1 · 2^{1} + b_0 · 2^{0}`; as we work towards the `Top` of the `PearTree`
we examine our index, so by the time examine our `k`'th constructor, we've
examined the `k` least significant bits of the index.

[BLAH BLAH BLAH - look at the skyline of the size]

We can even create a pseudooptic:

```haskell
at :: forall a. Int -> PearTree a -> Maybe (forall f. Functor f => (a -> f a) -> f (PearTree a))
at = loop id id Nothing where
  loop :: forall b. 
    (forall f. Functor f => (a -> f a) -> b -> f b) -> 
    (PearTree b -> PearTree a) -> 
    Maybe (forall f. Functor f => (a -> f a) -> f (PearTree a)) -> 
    Int -> 
    PearTree b -> 
    Maybe (forall f. Functor f => (a -> f a) -> f (PearTree a))
  loop lens wrap next = \case
    t :>- mb -> \ix -> 
      let here = mb <&> \b f -> wrap . (t :>-) <$> lens f b
          (q,r) = ix `quotRem` 2
      in 
      case r of
        0 -> loop (_1 . lens) (wrap . (:>- mb)) (here <|> next)
        ~1 -> loop (_2 . lens) (wrap . (:>- mb)) (here *> next)
    Top b -> \case
      0 -> Just \f -> wrap . Top <$> lens f b
      1 -> next
      _ -> Nothing
```

# `fromList` / `toList`


# `Semigroup`

----

We can also create a size-indexed variant of `PearTree`; similar to the size-indexed variant of `[]`, with the advantage that many operations drop from linear to log time.

  [note]: *
    Or you could just use a newtype wrapped vector and integers for O(1) indexing if you wanted real speed and efficiency. But this is more fun, don't you think?
    * 
