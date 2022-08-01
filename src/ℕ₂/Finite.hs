{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE ViewPatterns #-}
module ℕ₂.Finite where

import GHC.Types (Constraint)

import ℕ₂.Function
import ℕ₂.Singleton.Known

type Finite :: (k -> *) -> Constraint
class Finite (fin :: k -> *) where
  embedding :: Known t => fin t ?-> k

type Fin_ :: forall k -> k -> *
type family Fin_ k = fin | fin -> k

type Fin :: k -> *
type Fin (t :: k) = Fin_ k t

embed :: forall k (fin :: k -> *) (t :: k). (Finite fin, Known t) => fin t -> k
embed = to embedding

select :: forall k (fin :: k -> *) (t :: k). (Finite fin, Known t) => k -> Maybe (fin t)
select = from embedding
