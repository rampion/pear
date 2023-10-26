module Pear.Positive.Kind where

import Data.Kind (Constraint, Type)
import Pear.Bit
import Pear.Bit.Kind qualified as Bit
import Pear.Positive

type KnownPositive :: Positive -> Constraint
class KnownPositive n where
  positiveValue :: Sing n
  
type Sing :: Positive -> Type
data Sing n where
  SObI :: Sing 'ObI
  (::.) :: Sing bs -> Bit.Sing b -> Sing (bs ':. b)

type Fin :: Positive -> Type
data Fin n where
  FTop :: Fin 'ObI
  FOff :: Fin (bs ':. 'I)
  (:!) :: Fin bs -> Bit -> Fin (bs ':. b)

type Succ :: Positive -> Positive
type Succ n = AddWithCarry n 'ObI 'O

type Plus :: Positive -> Positive -> Positive
type Plus n m = AddWithCarry n m 'O

type AddWithCarry :: Positive -> Positive -> Bit -> Positive
type family AddWithCarry n m b where
  AddWithCarry 'ObI 'ObI b = 'ObI ':. b

  AddWithCarry 'ObI (m ':. 'O) 'O = m ':. 'I
  AddWithCarry 'ObI (m ':. 'O) 'I = AddWithCarry 'ObI m 'O ':. 'O
  AddWithCarry 'ObI (m ':. 'I) 'O = AddWithCarry 'ObI m 'O ':. 'O
  AddWithCarry 'ObI (m ':. 'I) 'I = AddWithCarry 'ObI m 'O ':. 'I

  AddWithCarry (m ':. 'O) 'ObI 'O = m ':. 'I
  AddWithCarry (m ':. 'O) 'ObI 'I = AddWithCarry 'ObI m 'O ':. 'O
  AddWithCarry (m ':. 'I) 'ObI 'O = AddWithCarry 'ObI m 'O ':. 'O
  AddWithCarry (m ':. 'I) 'ObI 'I = AddWithCarry 'ObI m 'O ':. 'I

  AddWithCarry (n ':. 'O) (m ':. 'O) b = AddWithCarry n m 'O ':. b
  AddWithCarry (n ':. 'O) (m ':. 'I) 'O = AddWithCarry n m 'O ':. 'I
  AddWithCarry (n ':. 'O) (m ':. 'I) 'I = AddWithCarry n m 'I ':. 'O
  AddWithCarry (n ':. 'I) (m ':. 'O) 'O = AddWithCarry n m 'O ':. 'I
  AddWithCarry (n ':. 'I) (m ':. 'O) 'I = AddWithCarry n m 'I ':. 'O
  AddWithCarry (n ':. 'I) (m ':. 'I) 'O = AddWithCarry n m 'I ':. 'O
  AddWithCarry (n ':. 'I) (m ':. 'I) 'I = AddWithCarry n m 'I ':. 'I
