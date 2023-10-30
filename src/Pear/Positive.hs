module Pear.Positive 
  ( module Pear.Positive
  , module Pear.Bit
  ) where

import Data.Kind (Type)
import Numeric.Natural (Natural)
import Pear.Bit

-- codata is two's complement notation (well, a subset of the 2-adics, really
type Positive :: Type
data Positive = ObI | Positive :. Bit
  deriving Eq
infixl 9 :. 

instance Show Positive where
  showsPrec p = showParen (p >= 9) . showsPositive where
    showsPositive :: Positive -> ShowS
    showsPositive = \case
      ObI -> showString "ObI"
      bs :. b -> showsPositive bs . showString " :. " . showsPrec 9 b

toNatural :: Positive -> Natural
toNatural = undefined

fromNatural :: Natural -> Maybe Positive
fromNatural = undefined
