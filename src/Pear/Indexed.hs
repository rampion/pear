{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
-- |
-- Defines an indexed functor typeclass, 'IFunctor', which allows a transform
-- based on both index and value, and extends this with indexed variants of
-- @Applicative@ ('IApplicative'), @Monad@ ('IMonad'), @Foldable@
-- ('IFoldable'), and @Traversable@ ('ITraversable').
--
-- The implementations of the indexed and non-indexed variants are very
-- similar, and can often be derived from each other.
--
-- -  Use @deriving anyclass@ to derive indexed variants from non-indexed
--    variants:
--
--      @
--      data X a = ...
--         deriving anyclass (IApplicative, IMonad, IFoldable, ITraversable)
--      @
--
-- -  Use 'Pear.Via.Deindexed' and @DerivingVia@ to derive non-indexed variants
--    from indexed variants.
--
--      @
--      data X a = ...
--         deriving (Functor, Applicative, Monad, Foldable, Traversable) via Deindexed X
--      @
--
-- -  Use 'Pear.Via.Elem' and @DerivingVia@ to derive instances from subclass instances.
--
--      - e.g. derive 'IFunctor', 'IApplicative' based on an implementation of
--        'IMonad'
--
--          @
--          data X a
--              deriving IFunctor via X ∈ IMonad
--
--          instance IApplicative X where
--            ipure = ...
--            liftIA2 = liftIM2
--          @
--
--      - e.g. derive 'IFunctor', 'IFoldable' based on an implementation of 'ITraversable'
--
--          @
--          data X a
--              deriving (IFunctor, IFoldable) via X ∈ ITraversable
--          @
module Pear.Indexed where

import GHC.Types (Constraint)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity (..))
import Data.Coerce (coerce)
import Data.Function ((&))
import Control.Monad (join)
import Data.Foldable (fold)

-- | Addressable types
type Indexed :: (* -> *) -> Constraint
class Indexed f where
  type Ix f :: *
  -- | a van Laarhoven-style lens for the value at an index
  at :: Functor g => Ix f -> (a -> g a) -> f a -> g (f a)

-- | Extract the value at an index
(!!) :: Indexed f => f a -> Ix f -> a
as !! i = gets i id as

-- | Extract and transform the value at an index
gets :: forall f a b. Indexed f => Ix f -> (a -> b) -> f a -> b
gets = coerce do at @f @(Const b) @a

-- | Set the value at an index
set :: Indexed f => Ix f -> a -> f a -> f a
set i a = update i do const a

-- | Update the value at an index
update :: forall f a. Indexed f => Ix f -> (a -> a) -> f a -> f a
update = coerce do at @f @Identity @a

-- | @Functor@ with indexes
type IFunctor :: (* -> *) -> Constraint
class (Indexed f, Functor f) => IFunctor f where
  -- | transform a parameterized type based on both index and value
  imap :: (Ix f -> a -> b) -> f a -> f b

-- | @Applicative@ with indexes
type IApplicative :: (* -> *) -> Constraint
class (Applicative f, IFunctor f) => IApplicative f where
  -- | create values based on indexes
  ipure :: (Ix f -> a) -> f a
  ipure f = imap (&) do pure f

  -- | combine two values index-by-index
  liftIA2 :: (Ix f -> a -> b -> c) -> f a -> f b -> f c
  liftIA2 f ma mb = imap f ma <*> mb

-- | List all positions
enumerate :: IApplicative f => f (Ix f)
enumerate = ipure id

-- | implementation of 'imap' using 'liftIA2' and 'pure'
liftIA :: IApplicative f => (Ix f -> a -> b) -> f a -> f b
liftIA f = liftIA2 (const . f) (pure ())

-- | @Monad@ with indexes
type IMonad :: (* -> *) -> Constraint
class (Monad m, IApplicative m) => IMonad m where
  -- | @>>=@ with index
  ibind :: (Ix m -> a -> m b) -> m a -> m b
  ibind f = join . imap f

-- | implementation of 'liftIA2' using '>>='
liftIM2 :: (Monad f, IApplicative f) => (Ix f -> a -> b -> c) -> f a -> f b -> f c
liftIM2 f ma mb = ma >>= \a -> mb >>= \b -> ipure \i -> f i a b

-- | @Foldable@ with index
type IFoldable :: (* -> *) -> Constraint
class (Indexed t, Foldable t) => IFoldable t where
  -- | @foldMap@ with index
  ifoldMap :: Monoid m => (Ix t -> a -> m) -> t a -> m

  default ifoldMap :: IFunctor t => Monoid m => (Ix t -> a -> m) -> t a -> m
  ifoldMap f = fold . imap f

-- | @Traversable@ with index
type ITraversable :: (* -> *) -> Constraint
class (Traversable t, IFoldable t, IFunctor t) => ITraversable t where
  -- | @traverse@ with index
  itraverse :: Applicative f => (Ix t -> a -> f b) -> t a -> f (t b)
  itraverse f = sequenceA . imap f

-- | implementation of 'imap' using 'itraverse'
itmap :: forall t a b. ITraversable t => (Ix t -> a -> b) -> t a -> t b
itmap = coerce do itraverse @t @Identity @a @b

-- | implementation of 'ifoldMap' using 'itraverse'
itfoldMap :: forall t m a. (ITraversable t, Monoid m) => (Ix t -> a -> m) -> t a -> m
itfoldMap = coerce do itraverse @t @(Const m) @a
