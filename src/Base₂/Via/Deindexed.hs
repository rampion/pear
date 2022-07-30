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
module Base₂.Via.Deindexed where

import Data.Coerce (coerce)
import Base₂.Indexed

type Deindexed :: (* -> *) -> * -> *
newtype Deindexed f a = Deindexed { getDeindexed :: f a }

everywhere :: (a -> b) -> i -> a -> b
everywhere = const

instance IFunctor m => Functor (Deindexed m) where
  fmap :: forall a b. (a -> b) -> Deindexed m a -> Deindexed m b
  fmap = coerce do imap @m @a @b . everywhere

instance IApplicative m => Applicative (Deindexed m) where
  pure :: forall a. a -> Deindexed m a
  pure = coerce do ipure @m . const @a

  (<*>) :: forall a b. Deindexed m (a -> b) -> Deindexed m a -> Deindexed m b
  (<*>) = coerce do liftIA2 @m @(a -> b) (flip everywhere)

instance IMonad m => Monad (Deindexed m) where
  (>>=) :: forall a b. Deindexed m a -> (a -> Deindexed m b) -> Deindexed m b
  (>>=) = coerce do flip do ibind @m @a @b . everywhere

instance IFoldable t => Foldable (Deindexed t) where
  foldMap :: forall m a. Monoid m => (a -> m) -> Deindexed t a -> m
  foldMap = coerce do ifoldMap @t @m @a . everywhere
  
instance ITraversable t => Traversable (Deindexed t) where
  traverse :: forall f a b. Applicative f => (a -> f b) -> Deindexed t a -> f (Deindexed t b)
  traverse = coerce do (fmap Deindexed .) . itraverse @t @f @a @b . everywhere
