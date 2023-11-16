module Pear.Pair where

import Data.Foldable1 (Foldable1(..))
import Data.Kind (Type)
import Pear.Bit
import Pear.Zipper

type Pair :: Type -> Type
data Pair a = a :× a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance Foldable1 Pair where
  foldMap1 f (a₀ :× a₁) = f a₀ <> f a₁

{-# COMPLETE (:><) #-}
pattern (:><) :: a -> a -> Pair a
pattern a₀ :>< a₁ = a₀ :× a₁
infix 0 :><, :×

at :: Functor f => Bit -> (a -> f a) -> Pair a -> f (Pair a)
at = undefined

fst :: Pair a -> a
fst (a₀ :× _₁) = a₀

snd :: Pair a -> a
snd (_₀ :× a₁) = a₁

instance Zipperable Pair where
  data Context Pair a = Hole :?× a | a :×? Hole
    deriving (Show, Eq, Functor, Foldable, Traversable)

  fillContext = \case
    Hole :?× a₁ -> (:× a₁)
    a₀ :×? Hole -> (a₀ :×)

  traverseWithContext f (a₀ :× a₁) = liftA2 (:×)
    do f (Hole :?× a₁) a₀ 
    do f (a₀ :×? Hole) a₁

  stepForward noZipper withZipper = \case
    Hole :?× a₁ -> \a₀ -> withZipper (a₀ :×? Hole) a₁
    _₀ :×? Hole -> \_₁ -> noZipper

  stepBackward noZipper withZipper = \case
    Hole :?× _₁ -> \_₀ -> noZipper
    a₀ :×? Hole -> \a₁ -> withZipper (Hole :?× a₁) a₀

pattern (:?><) :: Hole -> a -> Context Pair a
pattern hole :?>< a₁ = hole :?× a₁

pattern (:><?) :: a -> Hole -> Context Pair a
pattern a₀ :><? hole = a₀ :×? hole

infix 5 :?×, :×?, :?><, :><?

{-# COMPLETE (:?×), (:><?) #-}
{-# COMPLETE (:?><), (:×?) #-}
{-# COMPLETE (:?><), (:><?) #-}

instance Traversable (Zipper Pair) where
  traverse f = \case
    Zipper { context = ca₁@(Hole :?× _₁), value = a₀ } -> flip Zipper <$> f a₀ <*> traverse f ca₁
    Zipper { context = ca₀@(_₀ :×? Hole), value = a₁ } -> Zipper <$> traverse f ca₀ <*> f a₁
