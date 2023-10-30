module Pear.Zipper where

import Data.Kind (Type, Constraint)
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

-- | A lens for the focused value of a zipper
focus :: Lens' a (Zipper t a)
focus f Zipper{context,value} = Zipper context <$> f value

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
class (Functor t, Foldable t => Foldable (Zipper t), Traversable t => Traversable (Zipper t)) => Zipperable t where
  -- | A @Context t a@ is the type of one-hole contexts for the type @t a@
  data Context t :: Type -> Type

  -- | map over the values and contexts
  mapWithContext :: (Context t a -> a -> r) -> t a -> t r

  -- | create a value by filling the hole in the context
  fillContext :: Context t a -> a -> t a

  -- | Step to the next position
  zipNext :: Zipper t a -> Maybe (Zipper t a)

  -- | Step to the previous position
  zipPrevious :: Zipper t a -> Maybe (Zipper t a)

{-
instance Zipperable Maybe where
  data Context Maybe = InJust

  mapWithContext f = fmap (f InJust)
  fillContext 
-}

-- | Create a value from the zipper
zipUp :: Zipperable t => Zipper t a -> t a
zipUp Zipper{context,value} = fillContext context value

-- | Create zippers for each position in a data structure
zipDown :: Zipperable t => t a -> t (Zipper t a)
zipDown = mapWithContext Zipper
