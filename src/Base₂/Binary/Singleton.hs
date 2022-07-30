{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Base₂.Binary.Singleton where

import Base₂.Bit.Singleton
import Base₂.Binary
import Base₂.Function
import Base₂.Singleton

type SBinary :: Binary -> *
data SBinary n where
  SOb :: SBinary 'Ob
  (::.) :: SBinary bs -> SBit b -> SBinary (bs ':. b)

instance Singleton SBinary where
  promotion = Function
    { image = \case
        Ob -> Exists SOb
        (promote -> Exists bs) :. (promote -> Exists b) -> Exists (bs ::. b)
    , preimage = \case
        Exists SOb -> Ob
        Exists ((demote -> bs) ::. (demote -> b)) -> bs :. b
    }

type instance Sing_ Binary = SBinary
