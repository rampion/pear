-- |
-- A simplified version of @singletons@' @Data.Singletons@, this module defines a
-- family, 'Sing_', for mapping types to their singleton type and a class,
-- 'Singleton', for mapping values between the base type and the singleton type.
--
-- Given a base type, like @T@:
--
-- >>> :set -XStandaloneKindSignatures -XGADTs -XDataKinds -XTypeFamilies
-- >>> import Pear.Function (Function(..))
-- >>> :{
--        type T :: Type
--        data T = Ca | Cb | Cc
--     :}
--
-- @ST@ is a valid singleton for @T@ as each value @t :: T@ corresponds to
-- exactly one type @ST t@ and that type has a single inhabitant.
--
-- >>> :{
--        type ST :: T -> Type
--        data ST t where
--          SCa :: ST 'Ca
--          SCb :: ST 'Cb
--          SCc :: ST 'Cc
--     :}
--
-- Using the 'Singleton' class we can demonstrate this bijection between
-- values of @T@ and the singleton inhabitants..
--
-- >>> :{
--        instance Singleton ST where
--          promotion = Function
--            { image = \case
--                Ca -> Exists SCa
--                Cb -> Exists SCb
--                Cc -> Exists SCc
--            , preimage = \case
--                Exists SCa -> Ca
--                Exists SCb -> Cb
--                Exists SCc -> Cc
--            }
--      :}
--
-- There are many possible different singleton types for @T@, but we 
-- can use @Sing_@ to mark this as the preferred singleton for @T@.
--
-- >>> :{
--        type instance Sing_ T = ST
--     :}
--
--  The type alias 'Sing' is useful for translating promoted constructors to
--  their corresponding singleton types.
--
--  >>> :t SCa :: Sing 'Ca
--  SCa :: Sing 'Ca :: ST 'Ca
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

import GHC.Types (Constraint, Type)
import Pear.Function (type (<->), to, from)

-- | 
-- A singleton is a parameterized type, creating
-- distinct types for each value in the lifted type.
type Sing_ :: forall k -> k -> Type
type family Sing_ k = sing | sing -> k

-- |
-- Convenience alias for the 'Sing_' using the provided type to infer the kind.
type Sing :: k -> Type
type Sing (t :: k) = Sing_ k t

-- |
-- An instance of Singleton promises that each
-- singleton type has exactly one value.
type Singleton :: (k -> Type) -> Constraint
class Singleton (sing :: k -> Type) where
  -- | a bijection between a value and its singleton type
  promotion :: k <-> Exists sing

-- |
-- General-purpose existential wrapper type
type Exists :: (k -> Type) -> Type
data Exists f where
  Exists :: f a -> Exists f

-- |
-- Extract a result from an existential.
withExists :: (forall a. f a -> r) -> Exists f -> r
withExists g (Exists fa) = g fa

-- |
-- Convert a value into its singleton.
promote :: Singleton (sing :: k -> Type) => k -> Exists sing
promote = to promotion

-- |
-- Use the singleton version of a value in a calculation
promotes :: Singleton (sing :: k -> Type) => (forall t. sing t -> r) -> k -> r
promotes f = withExists f . promote 

-- |
-- Convert a singleton into the base type's value.
demote :: Singleton (sing :: k -> Type) => sing t -> k
demote = from promotion . Exists
