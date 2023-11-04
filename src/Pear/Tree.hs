module Pear.Tree 
  ( module Pear.Tree
  , module Pear.Positive
  , module Pear.Pair.LinkToModuleDocumentation
  , module Pear.Zipper
  ) where

import Prelude hiding (lookup, fst, snd, reverse)
import Control.Applicative (liftA2, liftA3)
import Control.Monad.State (evalState, state)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Const (pattern Const, getConst)
import Data.Functor.Identity (pattern Identity, runIdentity)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Numeric.Natural (Natural)
import Pear.Pair hiding (at)
import Pear.Pair qualified as Pear.Pair.LinkToModuleDocumentation -- avoid transcluding definitions from Pear.Pair when generating the documentation
import Pear.Positive
import Pear.Zipper

type Tree :: Type -> Type
data Tree a = Top a | Tree (Pair a) :>- Maybe a
infixl 4 :>-

deriving instance Eq a => Eq (Tree a)
deriving instance Functor Tree
deriving instance Foldable Tree
deriving instance Traversable Tree

instance Show a => Show (Tree a) where
  showsPrec p = showParen (p >= 4) . showsTree where
    -- *almost* equivalent to the derived instance, but the derived instance inserts
    -- unnecessary parentheses e.g.
    --
    --  (Top (('a' :× 'b') :× ('c' :× 'd')) :>- Nothing) :>- Just 'e'
    showsTree :: forall a. Show a => Tree a -> ShowS
    showsTree = \case
      Top a -> showString "Top " . showsPrec 10 a
      t :>- ma -> showsTree t . showString " :>- " . showsPrec 4 ma

instance Zipperable Tree where
  -- | one-hole contexts of 'Tree's
  data Context Tree a where
    -- Per Conor McBride, the one-hole context for a type is isomorphic to the
    -- derivative of the algebraic representation of a type.
    --
    -- @
    --  Maybe a                         ↔   1 + a
    --  Hole                            ↔   1
    --
    --  Top a                           ↔   a
    --  Tree (Pair a) :>- Maybe a       ↔   Tree (Pair a) · (1 + a)
    --  Tree a                          ↔   a + Tree (Pair a) · (1 + a)
    --
    --  Context Tree a                  ↔   d(Tree a)/da
    --                                  ↔   da/da + d(Tree (Pair a))/da · (1 + a) + Tree (Pair a) . d(1 + a)/da
    --
    --  da/da                           ↔   1
    --                                  ↔   AtTop
    --
    --  d(Tree (Pair a))/da · (1 + a)   ↔   Context Tree a² · Context Pair a · (1 + a)
    --                                  ↔   Context Tree a² :\ (Context Pair a, Maybe a)
    --
    --  Tree (Pair a) . d(1 + a)/da     ↔   Tree (Pair a) · 1
    --                                  ↔   Tree a² :\- Hole
    -- @
    --
    -- The chain rule is your friend
    AtTop :: Context Tree a
    (:\) :: Context Tree (Pair a) -> (Context Pair a, Maybe a)-> Context Tree a
    (:\-) :: Tree (Pair a) -> Hole -> Context Tree a
    deriving (Show, Eq, Functor)

  fillContext = \case
    AtTop             -> Top
    ta² :\- Hole      -> \a -> ta² :>- Just a
    cta² :\ (ca², ma) -> \a -> fillContext cta² (fillContext ca² a) :>- ma

  mapWithContext f = \case
    Top a -> Top (f AtTop a)
    ta² :>- ma -> 
      mapWithContext (\cta² -> mapWithContext \ca² -> f (cta² :\ (ca², ma))) ta² 
        :>- fmap (f (ta² :\- Hole)) ma
    
  stepForward withTree withZipper = \case
    AtTop -> withTree . Top
    ta²  :\- Hole -> withTree . \a -> ta² :>- Just a
    cta² :\ (ca², ma) -> 
      ca² & stepForward
        do cta² & stepForward
            do \ta² -> maybe (withTree (ta² :>- Nothing)) (withZipper (ta² :\- Hole)) ma
            do \cta² (a₂ :× a₃) -> withZipper (cta² :\ (Hole :< a₃, ma)) a₂
        do \ca² -> withZipper (cta² :\ (ca², ma))

  stepBackward = \withTree withZipper -> \case
    AtTop -> withTree . Top
    ta²  :\- Hole -> \a -> zipToLast (withZipper `onSnd` Just a) ta²
    cta² :\ (ca², ma) -> 
      ca² & stepBackward
        do cta² & stepBackward
            do \ta² -> withTree (ta² :>- ma)
            do withZipper `onSnd` ma
        do \ca² -> withZipper (cta² :\ (ca², ma))
    where
      onSnd :: (Context Tree a -> a -> r) -> Maybe a -> Context Tree (Pair a) -> Pair a -> r
      onSnd k ma cta² (a₀ :× a₁) = k (cta² :\ (a₀ :> Hole, ma)) a₁
      
      zipToLast :: (Context Tree a -> a -> r) -> Tree a -> r
      zipToLast k = \case
        Top a -> k AtTop a
        ta² :>- Just a -> k (ta² :\- Hole) a
        ta² :>- Nothing -> zipToLast (k `onSnd` Nothing) ta²

infixl 4 :\, :\-

instance Traversable (Zipper Tree) where
  traverse = setup where 

    setup :: Applicative f => (a -> f b) -> Zipper Tree a -> f (Zipper Tree b)
    setup g Zipper{context,value} = loop g 
      do g value <&> \b ctb² () -> Zipper ctb² b
      do context 
      do pure ()

    -- aggregate the effects so that the elements are traversed in the proper order
    loop 
      :: Applicative f 
      => (a -> f b) 
      -> f (Context Tree b -> x -> r) 
      -> Context Tree a -> f x -> f r 
    loop g fh = \case
      -- fh - Seeded with the focused element; used to grow the effects *out* around
      --      that point as we ascend through the balanced binary tree until we hit
      --      Top or a (:>-). 
      --
      --      For example consider the following partial context:
      --      
      --      … :\ (((A :× B) :× (C :× D)) :> Hole, _) 
      --        :\ (Hole :< (G :× H), _) 
      --        :\ (E :> Hole, _)
      --
      --      This can also be drawn as a tree diagram:
      --
      --                           :<
      --                 :>                     :×
      --          A :× B    C :> D    E :× Hole    G :× H
      --
      --      In this case, fh would be seeded with the focused element,
      --      then the effects of `E` would be placed before it,
      --      then the effects of `G :× H` placed after,
      --      then the effects of `(A :× B) :× (C :× D)` placed before
      --
      --      We use the function value to rearrange the values obtained
      --      from the effects in the order needed to recreate a Zipper.
      --
      -- fx - Offshoots from (:>-) below the focused balanced binary tree,
      --      effects are aggregated leftward
      --
      --      For example consider the following partial context:
      --
      --      … :\ (_, Just ((I :× J) :× (K :× L))) 
      --        :\ (_, Nothing)
      --        :\ (_, Just M)
      --
      --      This can also be drawn as a set of tree diagrams.
      --      
      --               Just         Nothing         Just
      --               :×
      --        I :× J    K :× L                    M
      --
      --      In this case, fx would start out empty,
      --      then the effects of `M` would be prepended to it,
      --      then the effects of `(I :× J) :× (K :× L)` would be prepended to
      --      it.
      --
      AtTop -> liftA2 
        do \h -> h AtTop
        do fh
      ta² :\- Hole -> liftA3 
        do \tb² h -> h (tb² :\- Hole)
        do traverse (both g) ta²
        do fh
      cta² :\ (Hole :< a₁, Nothing) -> loop (both g) 
        do (fh ? g a₁) \h b₁ ctb² -> h (ctb² :\ (Hole :< b₁, Nothing))
        do cta²
      cta² :\ (Hole :< a₁, Just a) -> \fx -> loop (both g)
        do (fh ? g a₁) \h b₁ ctb² k -> k h b₁ ctb²
        do cta²
        do (g a ? fx) \b x h b₁ ctb² -> h (ctb² :\ (Hole :< b₁, Just b)) x
      cta² :\ (a₀ :> Hole, Nothing) -> loop (both g)
        do (g a₀ ? fh) \b₀ h ctb² -> h (ctb² :\ (b₀ :> Hole, Nothing))
        do cta²
      cta² :\ (a₀ :> Hole, Just a) -> \fx -> loop (both g)
        do (g a₀ ? fh) \b₀ h ctb² k -> k b₀ h ctb²
        do cta²
        do (g a ? fx) \b x b₀ h ctb² -> h (ctb² :\ (b₀ :> Hole, Just b)) x

    -- infix version of liftA2
    (?) :: Applicative f => f a -> f b -> (a -> b -> c) -> f c
    (?) fa fb g = liftA2 g fa fb

    -- raises a kleisli arrow to operate on pairs
    both :: Applicative f => (a -> f b) -> Pair a -> f (Pair b)
    both g (a₀ :× a₁) = liftA2 (:×) (g a₀) (g a₁)

instance Semigroup (Tree a) where
  t₀ <> t₁ = foldl (flip push) t₀ t₁

type Tree0 :: Type -> Type
type Tree0 a = Maybe (Tree a)

reverse :: Tree a -> Tree a
reverse = loop id id where
  loop  :: ((a -> r) -> a -> r) -> (Tree a -> r) -> Tree a -> r
  loop f k = \case
    Top a -> f (k . Top) a
    ta² :>- Nothing -> loop
      do \g (a₀ :× a₁) -> f (\a₁ -> f (g . (a₁ :×)) a₀) a₁
      do \ta² -> k (ta² :>- Nothing)
      do ta²
    ta² :>- Just a -> loop
      do \g (a₀ :× a₁) -> f \a₂ -> f (\a₁ -> g (a₂ :× a₁) a₀) a₁
      do \ta² -> f \a -> k (ta² :>- Just a)
      do ta²
      do a

reverse0 :: Tree0 a -> Tree0 a
reverse0 = fmap reverse

filter :: (a -> Bool) -> Tree a -> Tree0 a
filter p = mapMaybe \a -> if p a then Just a else Nothing

mapMaybe :: (a -> Maybe b) -> Tree a -> Tree0 b
mapMaybe = undefined

size :: Tree a -> Positive
size =  \case
  Top _ -> ObI
  ta² :>- ma -> size ta² :. maybe O (const I) ma

size0 :: Tree0 a -> Natural
size0 = maybe 0 (toNatural . size)

head :: Tree a -> a
head = loop id where
  loop :: (b -> a) -> Tree b -> a
  loop k = \case
    Top b -> k b
    ta² :>- _ -> loop (k . fst) ta²

last :: Tree a -> a
last = loop id where
  loop :: (b -> a) -> Tree b -> a
  loop k = \case
    Top b -> k b
    ta² :>- Nothing -> loop (k . snd) ta²
    _ :>- Just a -> k a

(??) :: Tree a -> Natural -> Maybe a
(??) = flip \i -> fmap getConst . at i Const
infix 9 ??

put :: Natural -> a -> Tree a -> Maybe (Tree a)
put i = modify i . const

modify :: Natural -> (a -> a) -> Tree a -> Maybe (Tree a)
modify i f = fmap runIdentity . at i (Identity . f)

at :: Natural -> (forall f. Functor f => (a -> f a) -> Tree a -> Maybe (f (Tree a)))
at i f t = fmap zipUp . focus f <$> zipDown t ?? i

indexes :: Tree a -> Tree (Natural, a)
indexes = (`evalState` 0). traverse \a -> state \(!i) -> ((i, a), i + 1)

singleton :: a -> Tree a
singleton = Top

push :: a -> Tree a -> Tree a
push a = \case
  Top a₀ -> Top (a₀ :× a) :>- Nothing
  ta² :>- Nothing -> ta² :>- Just a
  ta² :>- Just a₀ -> push (a₀ :× a) ta² :>- Nothing

push0 :: a -> Tree0 a -> Tree a
push0 = liftA2 maybe Top push

pop :: Tree a -> (Tree0 a, a)
pop = \case 
  Top a -> (Nothing, a)
  ta² :>- Just a -> (Just (ta² :>- Nothing), a)
  ta² :>- Nothing ->
    let ~(ta, a) = pop2 ta² in (Just ta, a)

pop2 :: Tree (Pair a) -> (Tree a, a)
pop2 = \case
  Top (a₀ :× a₁) -> (Top a₀, a₁)
  ta⁴ :>- Just (a₀ :× a₁) -> (ta⁴ :>- Nothing :>- Just a₀, a₁)
  ta⁴ :>- Nothing -> 
    let ~(ta², a₀ :× a₁) = pop2 ta⁴ in (ta² :>- Just a₀, a₁)

split :: Positive -> Tree a -> Maybe (Tree a, Tree a)
split = undefined

split0 :: Natural -> Tree a -> (Tree0 a, Tree0 a)
split0 = undefined

fuse :: Tree a -> Tree a -> Tree a
fuse = undefined

fuse0 :: Tree0 a -> Tree0 a -> Tree0 a
fuse0 = liftA2 fuse

fiss :: Positive -> Tree a -> Maybe (Tree a, Tree a)
fiss = undefined

fiss0 :: Natural -> Tree a -> (Tree0 a, Tree0 a)
fiss0 = undefined

fromNonEmpty :: NonEmpty a -> Tree a
fromNonEmpty = undefined

fromList :: [a] -> Tree0 a
fromList = fmap fromNonEmpty . NonEmpty.nonEmpty

toNonEmpty :: Tree a -> NonEmpty a
toNonEmpty = undefined

toList :: Tree0 a -> [a]
toList = maybe [] (NonEmpty.toList . toNonEmpty)

generate :: Positive -> (Natural -> a) -> Tree a
generate = undefined

generate0 :: Natural -> (Natural -> a) -> Tree0 a
generate0 = maybe (const Nothing) (fmap Just . generate) . fromNatural

replicate :: Positive -> a -> Tree a
replicate n = generate n . const

replicate0 :: Natural -> a -> Tree0 a
replicate0 n = generate0 n . const
