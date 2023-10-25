{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveTraversable #-}
module Pear.Pair where

import Data.Functor ((<&>))

data Pair a = a :× a 
  deriving (Show, Eq, Functor, Foldable, Traversable)

infix 5 :×

instance Applicative Pair where
  pure a = a :× a
  (f₀ :× f₁) <*> (a₀ :× a₁) = f₀ a₀ :× f₁ a₁

data Bit = O | I deriving (Show, Eq, Ord, Enum)

at :: Functor f => Bit -> (a -> f a) -> Pair a -> f (Pair a)
at O f (a₀ :× a₁) = f a₀ <&> (:× a₁)
at I f (a₀ :× a₁) = (a₀ :×) <$> f a₁
