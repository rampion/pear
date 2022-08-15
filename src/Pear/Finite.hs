{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Pear.Finite where

import GHC.Types (Constraint)

import Pear.Function
import Pear.Singleton.Known

-- |
-- A `Finite` type contains subsets of its parameterized type.
--
-- Law:
--    from embedding . to embedding = Just
--
--    case from embedding t of
--      Nothing -> True
--      Just v -> to embedding v == t
--
type Finite :: (k -> *) -> Constraint
class Finite (fin :: k -> *) where
  -- | an injection from the finite type to its paramterized type
  embedding :: Known t => fin t ?-> k

-- |
-- A canonical finite type for a given parameterized type
type Fin_ :: forall k -> k -> *
type family Fin_ k = fin | fin -> k

-- |
-- A convenience alias for a finite type
type Fin :: k -> *
type Fin (t :: k) = Fin_ k t

-- |
-- mapping from a finite type to its paramterized type
embed :: forall k (fin :: k -> *) (t :: k). (Finite fin, Known t) => fin t -> k
embed = to embedding

-- |
-- partial mapping to a finite type from its parameterized type
select :: forall k (fin :: k -> *) (t :: k). (Finite fin, Known t) => k -> Maybe (fin t)
select = from embedding
