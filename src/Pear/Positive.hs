module Pear.Positive where

import Data.Kind (Type)
import Numeric.Natural (Natural)
import Pear.Bit

-- codata is two's complement notation (well, a subset of the 2-adics, really
type Positive :: Type
data Positive = ObI | Positive :. Bit

toNatural :: Positive -> Natural
toNatural = undefined

fromNatural :: Natural -> Maybe Positive
fromNatural = undefined
