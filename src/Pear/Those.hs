module Pear.Those
  ( module Pear.Those
  , module Data.These
  ) where

import Data.Kind (Type)
import Data.These

type Those :: Type -> Type -> Type
type Those a b = Maybe (These a b)

fromThose :: Those a b -> (Maybe a, Maybe b)
fromThose = maybe (Nothing, Nothing) \case
  This a -> (Just a, Nothing)
  That b -> (Nothing, Just b)
  These a b -> (Just a, Just b)

toThose :: (Maybe a, Maybe b) -> Those a b
toThose = \case
  (Nothing, Nothing) -> Nothing
  (Nothing, Just b) -> Just (That b)
  (Just a, Nothing) -> Just (This a)
  (Just a, Just b) -> Just (These a b)
