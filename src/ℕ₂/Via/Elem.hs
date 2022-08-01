{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
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
module ℕ₂.Via.Elem where

import GHC.Types (Constraint)
import Data.Coerce (coerce)
import ℕ₂.Indexed
import ℕ₂.Via.Deindexed


type (∈) :: k -> (k -> Constraint) -> k
data family a ∈ c 
newtype instance (f ∈ c) a = Elem1 { runElem1 :: f a }
newtype instance (f ∈ c) t a = Elem2 { runElem2 :: f t a }

type (⊆) :: (k -> Constraint) -> (k -> Constraint) -> Constraint
type c ⊆ d = (forall x. c x => d x)

instance (c ⊆ Indexed, c f) => Indexed (f ∈ c) where
  type Ix (f ∈ c) = Ix f

  at = \i -> elem1 . at i where
    elem1 :: Functor g => (f a -> g (f a)) -> (f ∈ c) a -> g ((f ∈ c) a)
    elem1 f = fmap Elem1 . f . runElem1

deriving via Deindexed (f ∈ IApplicative) instance IApplicative f => Functor (f ∈ IApplicative)

instance IApplicative f => IFunctor (f ∈ IApplicative) where
  imap :: forall a b. (Ix f -> a -> b) -> (f ∈ IApplicative) a -> (f ∈ IApplicative) b
  imap = coerce do liftIA @f @a @b

deriving via Deindexed (t ∈ ITraversable) instance ITraversable t => Functor (t ∈ ITraversable)
deriving via Deindexed (t ∈ ITraversable) instance ITraversable t => Foldable (t ∈ ITraversable)

instance ITraversable t => IFunctor (t ∈ ITraversable) where
  imap :: forall a b. (Ix t -> a -> b) -> (t ∈ ITraversable) a -> (t ∈ ITraversable) b
  imap = coerce do itmap @t @a @b

instance ITraversable t => IFoldable (t ∈ ITraversable) where
  ifoldMap :: forall m a. Monoid m => (Ix t -> a -> m) -> (t ∈ ITraversable) a -> m
  ifoldMap = coerce do itfoldMap @t @m @a
