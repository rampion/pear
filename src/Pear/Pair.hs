module Pear.Pair where

import Data.Kind (Type)
import Pear.Bit

type Pair :: Type -> Type
data Pair a = a :× a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

at :: Functor f => Bit -> (a -> f a) -> Pair a -> f (Pair a)
at = undefined

fst :: Pair a -> a
fst (a₀ :× _₁) = a₀

snd :: Pair a -> a
snd (_₀ :× a₁) = a₁
