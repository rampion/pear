{-# LANGUAGE TypeOperators #-}
module Pear.Opt where

import Data.Kind (Type)
import Pear.Bit
import Pear.Bit.Kind
import Pear.Selected

type Opt :: Bit -> Type -> Type
data Opt b a where
  None :: Opt 'O a
  Some :: a -> Opt 'I a

deriving instance Show a => Show (Opt b a)
deriving instance Eq a => Eq (Opt b a)
deriving instance Functor (Opt b)
deriving instance Foldable (Opt b)
deriving instance Traversable (Opt b)

at :: Fin b -> Opt b a -> a :← Opt b a
at = undefined
