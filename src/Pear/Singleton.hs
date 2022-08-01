{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Pear.Singleton where

import GHC.Types (Constraint)
import Pear.Function

type Singleton :: (k -> *) -> Constraint
class Singleton (sing :: k -> *) where
  promotion :: k <-> Exists sing

type Sing_ :: forall k -> k -> *
type family Sing_ k = sing | sing -> k

type Sing :: k -> *
type Sing (t :: k) = Sing_ k t

type Exists :: (k -> *) -> *
data Exists f where
  Exists :: f a -> Exists f

withExists :: (forall a. f a -> r) -> Exists f -> r
withExists g (Exists fa) = g fa

promote :: Singleton (sing :: k -> *) => k -> Exists sing
promote = to promotion

promotes :: Singleton (sing :: k -> *) => (forall t. sing t -> r) -> k -> r
promotes f = withExists f . promote 

demote :: Singleton (sing :: k -> *) => sing t -> k
demote = from promotion . Exists
