{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module Base₂.Singleton.Known where

import GHC.Types (Constraint)

import Base₂.Bit
import Base₂.Bit.Singleton
import Base₂.Binary
import Base₂.Binary.Singleton
import Base₂.Singleton

type Known :: k -> Constraint
class Singleton (Sing_ k) => Known (t :: k) where
  sing_ :: Sing_ k t

sing :: Known t => Sing t
sing = sing_

instance Known 'O where
  sing_ = SO

instance Known 'I where
  sing_ = SI

instance Known 'Ob where
  sing_ = SOb

instance (Known bs, Known b) => Known (bs ':. b) where
  sing_ = sing @bs ::. sing @b
