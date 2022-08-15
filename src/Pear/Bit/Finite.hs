{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Pear.Bit.Finite where

import Pear.Bit
import Pear.Bit.Singleton
import Pear.Finite
import Pear.Singleton.Known
import Pear.Function

-- |
-- A finite type for 'Bit'
--
-- - FBit 'O is isomorphic to 'Void'
-- - FBit 'I is isomorphic to '()'
type FBit :: Bit -> *
data FBit b where
  Z :: FBit 'I

type instance Fin_ Bit = FBit

instance Finite FBit where
  embedding = Function { image, preimage = preimage' sing } where
    image :: FBit t -> Bit
    image Z = O

    preimage' :: SBit t ->  Bit -> Maybe (Fin t)
    preimage' SO = const Nothing
    preimage' SI = \case
      O -> Just Z
      I -> Nothing

instance Show (FBit b) where
  showsPrec _ Z = showString "Z"

instance Eq (FBit b) where
  Z == Z = True
  
instance Ord (FBit b) where
  compare Z Z = EQ
