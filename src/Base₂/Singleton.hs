{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module Base₂.Singleton where

import GHC.Types (Constraint)
import Base₂.Function

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
