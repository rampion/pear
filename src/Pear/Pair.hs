module Pear.Pair where

import Data.Kind (Type)
import Pear.Bit

type Pair :: Type -> Type
data Pair a = a :× a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

at :: Functor f => Bit -> (a -> f a) -> Pair a -> f (Pair a)
at = undefined
