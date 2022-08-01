{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module ℕ₂.Bit.Singleton where

import ℕ₂.Bit
import ℕ₂.Function
import ℕ₂.Singleton

type SBit :: Bit -> *
data SBit b where
  SO :: SBit 'O
  SI :: SBit 'I

deriving instance Show (SBit b)
deriving instance Ord (SBit b)
deriving instance Eq (SBit b)

type instance Sing_ Bit = SBit

instance Singleton SBit where
  promotion = Function
    { image = \case
        O -> Exists SO
        I -> Exists SI
    , preimage = \case
        Exists SO -> O
        Exists SI -> I
    }
