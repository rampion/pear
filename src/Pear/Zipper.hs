-- | <https://wiki.haskell.org/Zipper Zippers> for use with Pear data structures.
module Pear.Zipper where

import Control.Applicative (liftA3)
import Control.Applicative.Backwards (pattern Backwards, forwards)
import Data.Functor.Const (pattern Const, getConst)
import Data.Functor.Identity (pattern Identity, runIdentity)
import Data.Kind (Type, Constraint)
import Data.Traversable (foldMapDefault)

-- | A read/write cursor into a data structure @t a@
type Zipper :: (Type -> Type) -> Type -> Type
data Zipper t a = Zipper
  { context :: Context t a -- ^ the currently focused position 
  , value :: a -- ^ the value at that position
  }

deriving instance (Show (Context t a), Show a) => Show (Zipper t a)
deriving instance (Eq (Context t a), Eq a) => Eq (Zipper t a)
deriving instance Functor (Context t) => Functor (Zipper t)
-- XXX: Zipper t a should also be a comonad, but I don't want to add the dependency

instance Zipperable t => Foldable (Zipper t) where
  foldMap = foldMapDefault

-- | Create a value from the zipper
zipUp :: Zipperable t => Zipper t a -> t a
zipUp Zipper{context,value} = fillContext context value

-- | Create zippers for each position in a data structure
zipDown :: Zipperable t => t a -> t (Zipper t a)
zipDown = mapWithContext Zipper

-- | Move the focus to the next position in the data structure
zipForward :: Zipperable t => Zipper t a -> Maybe (Zipper t a)
zipForward Zipper{context,value} = stepForward 
  do Nothing
  do \cta -> Just . Zipper cta
  do context
  do value

-- | Move the focus to the previous position in the data structure
zipBackward :: Zipperable t => Zipper t a -> Maybe (Zipper t a)
zipBackward Zipper{context,value} = stepBackward 
  do Nothing
  do \cta -> Just . Zipper cta
  do context
  do value

-- | Basic properties of a data structure's context
--
-- = Laws:
--
--  For any @Zipperable t@, where
--
--  * @ta :: t a@,
--  * @aᵢ :: a@ is the @i@'th element of @ta@, and
--  * @ctaᵢ :: Context t a@ is the context for the @i@'th position in @ta@
--
--  1. 'traverseWithContext' yields the context and value for each position in
--      the data structure in the same order as 'traverse'
--
--      > traverseWithContext (curry (Const . pure)) ta 
--      >   = Const [(cta₀, a₀), (cta₁, a₁), ...]
--
--  2. 'fillContext' reconstructs the original data structure from the context
--      for a position and the original value for that position
--
--      > fillContext ctaᵢ aᵢ = ta, 0 ≤ i < ‖ta‖
--
--  4. 'stepForward' follows the traversal order of the underlying data structure.
--
--      > stepForward Nothing (curry Just) ctaᵢ aᵢ 
--      >   | i + 1 < ‖ta‖  = Just (ctaᵢِ₊₁, aᵢ₊₁)
--      >   | otherwise     = Nothing
--
--  4. 'stepBackward' reverses the traversal order of the underlying data structure.
--
--      > stepBackward Nothing (curry Just) ctaᵢ aᵢ 
--      >   | 0 ≤ i - 1     = Just (ctaᵢ₋₁, aᵢ₋₁)
--      >   | otherwise     = Nothing
--
type Zipperable :: (Type -> Type) -> Constraint
class (Traversable t, Traversable (Zipper t), Functor (Context t)) => Zipperable t where

  -- | A @Context t a@ is the type of one-hole contexts for the type @t a@
  data Context t :: Type -> Type
  {-
  An alternate representation of contexts could use a type family that
  was parameterized by the type of the hole and the type of the value

    data family Ctx x a
    data instance Ctx a a = H
    data instance Ctx x (Pair a) = Ctx x a :< a | a :> Ctx x a
    data instance Ctx x (Tree a) where
      AtTop :: Ctx x a -> Ctx x (Tree a)
      (:\-) :: Ctx x (Tree (Pair a)) -> Maybe a -> Ctx x (Tree a)
      (:/-) :: Tree (Pair a) -> Ctx x a -> Ctx x (Tree a)

  Though this would give very pretty contexts for trees, e.g.

    AtTop (((a :× b) :> (H :< d)) :< ((e :× f) :× (g × h))) 
      :\- Nothing :\- Just (i :× j) :\- Nothing

    Top (((a :× b) :× (c :× d)) :× ((e :× f) :× (g × h))) 
      :>- Nothing :/- (H :< j) :\- Nothing

  It's not spiritually compatible with a classic zipper; moving the hole would
  require acting at depth, rather than acting near the top of the syntax tree.

  There's an isomorphism between `Context t a` and `Ctx a (t a)`, and both
  are derivatives of `t a` in terms of a.  `Ctx` does allow for a little
  more flexibility in terms of differentiation: `Ctx x a` is equivalent to da/dx.

  As a category, `Ctx` also lends itself to composition via substitution

    (.) :: Ctx y z -> Ctx x y -> Ctx x z

  Though `Context` is sufficient for the currentl implementation of this library,
  it mayb be worth considering `Ctx` as a replacement in the future.
   -}

  -- | A traversal of each position's context and value
  traverseWithContext :: Applicative f => (Context t a -> a -> f b) -> t a -> f (t b)

  -- | Create a value by filling the hole in the context
  fillContext :: Context t a -> a -> t a

  -- | Focus on the next value in the collection, if one exists
  --
  -- This uses continuation passing style; it's effectively equivalent to
  --
  -- > stepForward' :: Context t a -> a -> Maybe (Context t a, a)
  -- > stepForward' = stepForward Nothing (curry Just)
  stepForward :: r -> (Context t a -> a -> r) -> Context t a -> a -> r
  stepForward r _ _ _ = r

  -- | Focus on the previous value in the collection, if one exists
  --
  -- This uses continuation passing style; it's effectively equivalent to
  --
  -- > stepBackward' :: Context t a -> a -> Maybe (Context t a, a)
  -- > stepBackward' = stepBackward Nothing (curry Just)
  stepBackward :: r -> (Context t a -> a -> r) -> Context t a -> a -> r
  stepBackward r _ _ _ = r

-- | Map each element of the collection with its context
mapWithContext :: Zipperable t => (Context t a -> a -> b) -> t a -> t b
mapWithContext f = runIdentity . traverseWithContext (fmap Identity . f)

-- | Fold each element of the collection with its context
foldMapWithContext :: (Monoid m, Zipperable t) => (Context t a -> a -> m) -> t a -> m
foldMapWithContext f = getConst . traverseWithContext (fmap Const . f)

instance Zipperable Maybe where
  data Context Maybe a = InJust
    deriving (Show, Eq, Functor)

  traverseWithContext f = traverse (f InJust)
  fillContext InJust = Just

instance Traversable (Zipper Maybe) where
  traverse f = fmap (Zipper InJust) . f . value

instance Zipperable (Either e) where
  data Context (Either e) a = InRight
    deriving (Show, Eq, Functor)

  traverseWithContext f = traverse (f InRight)
  fillContext InRight = Right

instance Traversable (Zipper (Either e)) where
  traverse f = fmap (Zipper InRight) . f . value

instance Zipperable ((,) e) where
  data Context ((,) e) a = InSnd e
    deriving (Show, Eq, Functor)

  traverseWithContext f (e, a)= (e,) <$> f (InSnd e) a
  fillContext (InSnd e) a = (e, a)

instance Traversable (Zipper ((,) e)) where
  traverse f (Zipper (InSnd e) a) = Zipper (InSnd e) <$> f a

instance Zipperable [] where
  data Context [] a = ListContext { before :: [a], after :: [a] }
    deriving (Show, Eq, Functor)

  traverseWithContext f = loop [] where 
    loop before = \case
      a : after -> liftA2 (:) 
        do f ListContext{before,after} a
        do loop (a:before) after
      [] -> pure []

  fillContext ListContext{before,after} a = reverse before ++ a : after

  stepForward noZipper withZipper ListContext{before,after} = case after of
    next : after -> \here -> withZipper (ListContext (here:before) after) next
    [] -> const noZipper

  stepBackward noZipper withZipper ListContext{before,after} = case before of
    prev : before -> \here -> withZipper (ListContext before (here:after)) prev
    [] -> const noZipper

instance Traversable (Zipper []) where
  traverse f (Zipper ListContext{before,after} a) = 
    liftA3
      do \before b after -> Zipper ListContext{before,after} b
      do forwards (traverse (Backwards . f) before) 
      do f a
      do traverse f after

-- | A Van Laarhoven-style lens for the focused value of a zipper
focus :: Functor f => (a -> f a) -> Zipper t a -> f (Zipper t a)
focus f Zipper{context,value} = Zipper context <$> f value

-- | Stand in for misssing value in a context
data Hole = Hole
  deriving (Show, Eq)
