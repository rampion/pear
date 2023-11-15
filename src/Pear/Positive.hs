module Pear.Positive 
  ( Positive(..)
  , fromPositive
  , toPositive
  , literal
  , module Pear.Bit
  ) where

import Data.Kind (Type)
import Data.Proxy (pattern Proxy)
import GHC.TypeLits (KnownNat, type (<=), natVal)
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

-- | Convert a number from its @Positive@ representation to its @Natural@
-- representation
fromPositive :: Positive -> Natural
fromPositive = loop 1 0 where
  loop !twoⁿ !total = \case
    ObI -> twoⁿ + total
    bs :. b -> loop (2*twoⁿ) (total + bit 0 twoⁿ b) bs

-- | Convert a number from its @Natural@ representation to its @Positive@
-- representation, if possible
toPositive :: Natural -> Maybe Positive
toPositive = \case
  0 -> Nothing
  n -> Just (fromNonzero n)

-- | Create a @Positive@ value from a value known to be positive.
--
-- >>> literal @1
-- ObI
-- >>> literal @10
-- ObI :. O :. I :. O
literal :: forall n. (KnownNat n, 1 <= n) => Positive
literal = fromNonzero (natVal (Proxy @n))

-- | Helper for 'toPositive', 'literal'
fromNonzero :: Integral a => a -> Positive
fromNonzero = loop id where
  loop k = \case
    1 -> k ObI
    n ->
      let (q, r) = n `quotRem` 2
          b = if r == 0 then O else I
      in loop (k . (:. b)) q
