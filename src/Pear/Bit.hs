module Pear.Bit where

import Data.Kind (Type)

type Bit :: Type
data Bit = O | I
  deriving stock (Show, Read, Eq, Ord)
