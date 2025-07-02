# NOTES<!-- {{{ -->

3. Type-level _positive_ binary numbers

  - Zero makes Binary numbers with a unique representation hecka complicated, so let's ditch it for now

    footnote: If we need it later we can use a type level Maybe, call them Tree0 or NullableTree

  - Secret truth of indexed data types - the data type structure will mimic the index's

3.5: Note that the data structure mimics the shape of the index type, and that the algorithm for (++) mimics the implementation of Add

4. What do the powers of 2 look like?
5. Perfectly balanaced trees
6. Forest of balanced trees
7. Unindexed pear tree
8. Adding them
  - man I wish there were a way to say "any 3 of these 4 things uniquely identifies the fourth" but injective type families isn't it

8. Next episode (./Pear/Later.hs)
  - fizz isn't super-ergonomic, tune in next time for `lsplitAt` and `rsplitAt`, which require the binary subtraction algorithm
  - `joinTree :: Tree n (Tree m a) -> Tree (n * m) a` will let us do multiplication. `chunksOf :: SPositive d -> Tree n a -> (Tree (Quot n d) 
    a, Tree (Rem n a) a)`
  - I'm not going to do Fibonacci trees, even though it beats binary trees asymptotic behavior. At least not anytime soon.
    I've been knocking around drafts of this for almost 4 years, time to publish.
  - using SPositive to work with type-level binary; sequal, scompare, saddC, srminus, slminus (and why they're different)
    scommute

    it's so easy to get bogged down in playing with these. like trust me. so very very easy.
  - Dict c, Imp c = Dict c -> Void; can I make Imp actually useful?

<!-- }}} -->