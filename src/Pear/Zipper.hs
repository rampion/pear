module Pear.Zipper where

import Control.Applicative (liftA3)
import Control.Applicative.Backwards (pattern Backwards, forwards)
import Data.Kind (Type, Constraint)
import Data.Traversable (foldMapDefault)
import Pear.Lens

-- | Stand in for misssing value in a context
data Hole = Hole
  deriving (Show, Eq)

-- | A @Zipper t a@ is a read/write cursor into a data structure
type Zipper :: (Type -> Type) -> Type -> Type
data Zipper t a = Zipper
  { context :: Context t a -- ^ the currently focused position 
  , value :: a -- ^ the value at that position
  }

deriving instance (Show (Context t a), Show a) => Show (Zipper t a)
deriving instance (Eq (Context t a), Eq a) => Eq (Zipper t a)
deriving instance Functor (Context t) => Functor (Zipper t)

instance Zipperable t => Foldable (Zipper t) where
  foldMap = foldMapDefault

-- | A lens for the focused value of a zipper
focus :: Lens' a (Zipper t a)
focus f Zipper{context,value} = Zipper context <$> f value

-- | Create a value from the zipper
zipUp :: Zipperable t => Zipper t a -> t a
zipUp Zipper{context,value} = fillContext context value

-- | Create zippers for each position in a data structure
zipDown :: Zipperable t => t a -> t (Zipper t a)
zipDown = mapWithContext Zipper

-- | Step to the next position
zipForward :: Zipperable t => Zipper t a -> Maybe (Zipper t a)
zipForward Zipper{context,value} = stepForward 
  do const Nothing
  do \cta -> Just . Zipper cta
  do context
  do value

-- | Step to the previous position
zipBackward :: Zipperable t => Zipper t a -> Maybe (Zipper t a)
zipBackward Zipper{context,value} = stepBackward 
  do const Nothing
  do \cta -> Just . Zipper cta
  do context
  do value


-- | Basic operations for a zipper
--
-- laws:
--
--  1) zipDown gives you the zipper for each position
--  
--      value <$> zipDown t = t
--
--  2) zipUp reconstructs the value from the zipper
--
--      zipUp <$> zipDown t = const t <$> t
--
--  3) zipForward and zipBackward are opposites
--
--      zipForward za = Just zb ↔ Just za = zipBackward zb
--
--  4) t and Zipper t are traversed/folded in the same order
--
--  5) zipForward obeys traversal order
--
--      fmap value . zipForward <$> zipDown t 
--        = forwards (traverse (\a -> (Backwards . State) \s -> (s, Just a)) t) `evalState` Nothing
--
type Zipperable :: (Type -> Type) -> Constraint
class (Traversable t, Traversable (Zipper t), Functor (Context t)) => Zipperable t where
  -- Zipper t a should also be a comonad, but I don't want to add the dependency

  -- | A @Context t a@ is the type of one-hole contexts for the type @t a@
  data Context t :: Type -> Type

  -- | map over the values and contexts
  mapWithContext :: (Context t a -> a -> r) -> t a -> t r

  -- | create a value by filling the hole in the context
  fillContext :: Context t a -> a -> t a

  -- | advance to the next context if available
  stepForward :: (t a -> r) -> (Context t a -> a -> r) -> Context t a -> a -> r
  stepForward k _ cta = k . fillContext cta

  -- | advance to the previous context if available
  stepBackward :: (t a -> r) -> (Context t a -> a -> r) -> Context t a -> a -> r
  stepBackward k _ cta = k . fillContext cta

instance Zipperable Maybe where
  data Context Maybe a = InJust
    deriving (Show, Eq, Functor)

  mapWithContext f = fmap (f InJust)
  fillContext InJust = Just

instance Traversable (Zipper Maybe) where
  traverse f = fmap (Zipper InJust) . f . value

instance Zipperable (Either e) where
  data Context (Either e) a = InRight
    deriving (Show, Eq, Functor)

  mapWithContext f = fmap (f InRight)
  fillContext InRight = Right

instance Traversable (Zipper (Either e)) where
  traverse f = fmap (Zipper InRight) . f . value

instance Zipperable ((,) e) where
  data Context ((,) e) a = InSnd e
    deriving (Show, Eq, Functor)

  mapWithContext f (e, a)= (e, f (InSnd e) a) 
  fillContext (InSnd e) a = (e, a)

instance Traversable (Zipper ((,) e)) where
  traverse f (Zipper (InSnd e) a) = Zipper (InSnd e) <$> f a

instance Zipperable [] where
  data Context [] a = ListContext { before :: [a], after :: [a] }
    deriving (Show, Eq, Functor)

  mapWithContext f = loop [] where 
    loop before = \case
      a : after -> f ListContext{before,after} a : loop (a:before) after
      [] -> []

  fillContext ListContext{before,after} a = reverse before ++ a : after

  stepForward withList withZipper ListContext{before,after} = case after of
    next : after -> \here -> withZipper (ListContext (here:before) after) next
    [] -> \here -> withList (reverse (here:before))

  stepBackward withList withZipper ListContext{before,after} = case before of
    prev : before -> \here -> withZipper (ListContext before (here:after)) prev
    [] -> \here -> withList (here:after)

instance Traversable (Zipper []) where
  traverse f (Zipper ListContext{before,after} a) = 
    liftA3
      do \before b after -> Zipper ListContext{before,after} b
      do forwards (traverse (Backwards . f) before) 
      do f a
      do traverse f after

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
