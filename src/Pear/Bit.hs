{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Pear.Bit where

import GHC.Types (Type)
import GHC.Generics (Generic)
import Data.Bits (Bits(..))

-- |
-- A 0 or a 1
type Bit :: Type
data Bit 
    = O  -- ^ 0
    | I  -- ^ 1
  deriving (Eq, Ord, Show, Read, Enum, Generic, Bounded)

instance Bits Bit where
  I .&. I = I
  _ .&. _ = O
  O .|. O = O
  _ .|. _ = I
  I `xor` b = complement b
  O `xor` b = b
  complement O = I
  complement I = O
  shift b 0 = b
  shift _ _ = O
  rotate = const
  bitSize = const 1
  bitSizeMaybe = const (Just 1)
  isSigned = const False
  testBit I 0 = True
  testBit _ _ = False
  bit b
    | even b = O
    | otherwise = I
  popCount O = 0
  popCount I = 1
