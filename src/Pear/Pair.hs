module Pear.Pair where

import Data.Kind (Type)
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

  fillContext = \case
    Hole :< a₁ -> (:× a₁)
    a₀ :> Hole -> (a₀ :×)

  mapWithContext f (a₀ :× a₁) = 
    f (Hole :< a₁) a₀ :× f (a₀ :> Hole) a₁

  stepForward withPair withZipper = \case
    Hole :< a₁ -> \a₀ -> withZipper (a₀ :> Hole) a₁
    a₀ :> Hole -> \a₁ -> withPair (a₀ :× a₁)

  stepBackward withPair withZipper = \case
    Hole :< a₁ -> \a₀ -> withPair (a₀ :× a₁)
    a₀ :> Hole -> \a₁ -> withZipper (Hole :< a₁) a₀

infix 5 :<, :>

instance Traversable (Zipper Pair) where
  traverse f = \case
    Zipper { context = ca₁@(Hole :< _₁), value = a₀ } -> flip Zipper <$> f a₀ <*> traverse f ca₁
    Zipper { context = ca₀@(_₀ :> Hole), value = a₁ } -> Zipper <$> traverse f ca₀ <*> f a₁
