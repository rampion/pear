module Pear.Bit where

import Data.Kind (Type)

type Bit :: Type
data Bit = O | I
  deriving stock (Show, Read, Eq, Ord)

bit :: a -> a -> Bit -> a
bit o i = \case
  O -> o
  I -> i
