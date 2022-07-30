{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
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
{-# LANGUAGE TypeOperators #-}
module Base₂.Indexed where

import GHC.Types (Constraint)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity (..))
import Data.Coerce (coerce)
import Data.Function ((&))
import Control.Monad (join)
import Data.Foldable (fold)

type Indexed :: (* -> *) -> Constraint
class Indexed f where
  type Ix f :: *
  at :: Functor g => Ix f -> (a -> g a) -> f a -> g (f a)

(!!) :: Indexed f => f a -> Ix f -> a
as !! i = gets i id as

gets :: forall f a b. Indexed f => Ix f -> (a -> b) -> f a -> b
gets = coerce do at @f @(Const b) @a

set :: Indexed f => Ix f -> a -> f a -> f a
set i a = update i do const a

update :: forall f a. Indexed f => Ix f -> (a -> a) -> f a -> f a
update = coerce do at @f @Identity @a

type IFunctor :: (* -> *) -> Constraint
class (Indexed f, Functor f) => IFunctor f where
  imap :: (Ix f -> a -> b) -> f a -> f b

-- IFunctor suggests the existence of indexed variants of Applicative, Monad,
-- Foldable and Traversable.
--
-- To derive indexed variants from non-indexed variants:
--
--    data X a = ...
--       deriving anyclass (IApplicative, IMonad, IFoldable, ITraversable)
--
-- To derive non-indexed variants from indexed variants, see Base₂.Via.Deindexed
--
-- To derive instances from subclass instances, see Base₂.Via.Elem
--
--    data X a = ...
--       deriving (Functor, Applicative, Monad, Foldable, Traversable) via Deindexed X
--
-- To derive IFunctor, IApplicative based on an implementation of IMonad
--
--    data X a
--        deriving IFunctor via X ∈ IMonad
--
--    instance IApplicative X where
--      ipure = ...
--      liftIA2 = liftIM2
--
-- To derive IFunctor, IFoldable based on an implementation of ITraversable
--
--    data X a
--        deriving (IFunctor, IFoldable) via X ∈ ITraversable
--
type IApplicative :: (* -> *) -> Constraint
class (Applicative f, IFunctor f) => IApplicative f where
  ipure :: (Ix f -> a) -> f a
  ipure f = imap (&) do pure f

  liftIA2 :: (Ix f -> a -> b -> c) -> f a -> f b -> f c
  liftIA2 f ma mb = imap f ma <*> mb

enumerate :: IApplicative f => f (Ix f)
enumerate = ipure id

liftIA :: IApplicative f => (Ix f -> a -> b) -> f a -> f b
liftIA f = liftIA2 (const . f) (pure ())

type IMonad :: (* -> *) -> Constraint
class (Monad m, IApplicative m) => IMonad m where
  ibind :: (Ix m -> a -> m b) -> m a -> m b
  ibind f = join . imap f

liftIM2 :: (Monad f, IApplicative f) => (Ix f -> a -> b -> c) -> f a -> f b -> f c
liftIM2 f ma mb = ma >>= \a -> mb >>= \b -> ipure \i -> f i a b

type IFoldable :: (* -> *) -> Constraint
class (Indexed t, Foldable t) => IFoldable t where
  ifoldMap :: Monoid m => (Ix t -> a -> m) -> t a -> m

  default ifoldMap :: IFunctor t => Monoid m => (Ix t -> a -> m) -> t a -> m
  ifoldMap f = fold . imap f

type ITraversable :: (* -> *) -> Constraint
class (Traversable t, IFoldable t, IFunctor t) => ITraversable t where
  itraverse :: Applicative f => (Ix t -> a -> f b) -> t a -> f (t b)
  itraverse f = sequenceA . imap f

itmap :: forall t a b. ITraversable t => (Ix t -> a -> b) -> t a -> t b
itmap = coerce do itraverse @t @Identity @a @b

itfoldMap :: forall t m a. (ITraversable t, Monoid m) => (Ix t -> a -> m) -> t a -> m
itfoldMap = coerce do itraverse @t @(Const m) @a
