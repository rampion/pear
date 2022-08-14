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
module Pear.Singleton.Known where

import GHC.Types (Constraint)

import Pear.Bit
import Pear.Bit.Singleton
import Pear.Binary
import Pear.Binary.Singleton
import Pear.Singleton

-- |
-- A given type is 'Known' if the value of its singleton type can be
-- constructed.
type Known :: k -> Constraint
class Singleton (Sing_ k) => Known (t :: k) where
  sing_ :: Sing_ k t

-- | A convenience alias for `sing_` that
-- makes it easier to use with TypeApplications
--
-- >>> :set -XTypeApplications -XDataKinds
-- >>> sing_ @Bit @O
-- SO
-- >>> sing @O
-- SO
sing :: forall t. Known t => Sing t
sing = sing_

instance Known 'O where
  sing_ = SO

instance Known 'I where
  sing_ = SI

instance Known 'Ob where
  sing_ = SOb

instance (Known bs, Known b) => Known (bs ':. b) where
  sing_ = sing @bs ::. sing @b
