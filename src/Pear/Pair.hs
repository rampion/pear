module Pear.Pair where

import Data.Kind (Type)
import Data.Traversable (foldMapDefault)
import Pear.Bit
import Pear.Zipper

type Pair :: Type -> Type
data Pair a = a :× a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

at :: Functor f => Bit -> (a -> f a) -> Pair a -> f (Pair a)
at = undefined

fst :: Pair a -> a
fst (a₀ :× _₁) = a₀

snd :: Pair a -> a
snd (_₀ :× a₁) = a₁

instance Zipperable Pair where
  data Context Pair a = Hole :< a | a :> Hole
    deriving (Show, Eq, Functor, Foldable, Traversable)

  zipUp Zipper{context,value} = case context of
    Hole :< a₁ -> value :× a₁
    a₀ :> Hole -> a₀ :× value

  zipDown (a₀ :× a₁) = Zipper (Hole :< a₁) a₀ :× Zipper (a₀ :> Hole) a₁

  zipNext Zipper{context,value} = case context of
    Hole :< a₁ -> Just do Zipper (value :> Hole) a₁
    _₀ :> Hole -> Nothing

  zipPrevious Zipper{context,value} = case context of
    Hole :< _₁ -> Nothing
    a₀ :> Hole -> Just do Zipper (Hole :< value) a₀

infix 5 :<, :>

instance Foldable (Zipper Pair) where
  foldMap = foldMapDefault

instance Traversable (Zipper Pair) where
  traverse f = \case
    Zipper { context = ca₁@(Hole :< _₁), value = a₀ } -> flip Zipper <$> f a₀ <*> traverse f ca₁
    Zipper { context = ca₀@(_₀ :> Hole), value = a₁ } -> Zipper <$> traverse f ca₀ <*> f a₁
