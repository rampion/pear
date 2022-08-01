{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
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

type FBit :: Bit -> *
data FBit b where
  Z :: FBit 'I

{-
deriving instance Show Bit
deriving instance Enum Bit
deriving instance Ord Bit
deriving instance Eq Bit
-}

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
