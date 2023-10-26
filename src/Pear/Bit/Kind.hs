{-# LANGUAGE DataKinds #-}
module Pear.Bit.Kind where

import Pear.Bit
import Data.Kind (Constraint, Type)

type KnownBit :: Bit -> Constraint
class KnownBit b where
  bitValue :: Sing b

type Sing :: Bit -> Type
data Sing b where
  SO :: Sing 'O
  SI :: Sing 'I

type Fin :: Bit -> Type
data Fin b where
  FZ :: Fin 'I
