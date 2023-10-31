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
--  3) zipNext and zipPrevious are opposites
--
--      zipNext za = Just zb ↔ Just za = zipPrevious zb
--
--  4) t and Zipper t are traversed/folded in the same order
--
--  5) zipNext obeys traversal order
--
--      fmap value . zipNext <$> zipDown t 
--        = forwards (traverse (\a -> (Backwards . State) \s -> (s, Just a)) t) `evalState` Nothing
--
type Zipperable :: (Type -> Type) -> Constraint
class (Traversable t, Traversable (Zipper t), Functor (Context t)) => Zipperable t where
  -- | A @Context t a@ is the type of one-hole contexts for the type @t a@
  data Context t :: Type -> Type

  -- | map over the values and contexts
  mapWithContext :: (Context t a -> a -> r) -> t a -> t r

  -- | create a value by filling the hole in the context
  fillContext :: Context t a -> a -> t a

  -- | Step to the next position
  zipNext :: Zipper t a -> Maybe (Zipper t a)
  zipNext _ = Nothing

  -- | Step to the previous position
  zipPrevious :: Zipper t a -> Maybe (Zipper t a)
  zipPrevious _ = Nothing

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

  zipNext (Zipper ListContext{before,after} a) = case after of
    a' : after' -> Just (Zipper (ListContext (a:before) after') a')
    _ -> Nothing

  zipPrevious (Zipper ListContext{before,after} a) = case before of
    a' : before' -> Just (Zipper (ListContext before' (a:after)) a')
    _ -> Nothing

instance Traversable (Zipper []) where
  traverse f (Zipper ListContext{before,after} a) = 
    liftA3
      do \before b after -> Zipper ListContext{before,after} b
      do forwards (traverse (Backwards . f) before) 
      do f a
      do traverse f after
