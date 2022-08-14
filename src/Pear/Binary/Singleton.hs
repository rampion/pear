{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Pear.Binary.Singleton where

import Pear.Bit.Singleton
import Pear.Binary
import Pear.Function
import Pear.Singleton

-- | A 'Singleton' Type for 'Binary'
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
