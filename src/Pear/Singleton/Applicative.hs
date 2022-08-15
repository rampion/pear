{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Pear.Singleton.Applicative where

import GHC.Types (Constraint)
import Control.Applicative (liftA)
import Data.Coerce (coerce)

import Pear.Singleton
import Pear.Singleton.Known
import Pear.Via.Elem
import Pear.Finite
import Pear.Indexed

-- | An Applicative class for parameterized types that uses explicit singletons
-- (rather than Known) to make definition via recursion easier.
type SApplicative :: (k -> * -> *) -> Constraint
class SApplicative (f :: k -> * -> *) where
  -- | compare to 'ipure'
  spure :: Sing t -> (Fin t -> a) -> f t a
  -- | compare to 'liftIA2'
  liftSA2 :: Sing t -> (Fin t -> a -> b -> c) -> f t a -> f t b -> f t c

instance (SApplicative f, Known t) => Functor ((f ∈ SApplicative) t) where
  fmap = liftA

instance (SApplicative f, Known t) => Applicative ((f ∈ SApplicative) t) where
  pure = Elem2 . spure sing . const
  Elem2 ftg <*> Elem2 fta = Elem2 do liftSA2 sing (const id) ftg fta

instance (SApplicative f, Known t, Indexed (f t), Fin t ~ Ix (f t)) => Indexed ((f ∈ SApplicative) t) where
  type Ix ((f ∈ SApplicative) t) = Fin t
  at ix g (Elem2 fta) = Elem2 <$> at ix g fta
  
instance (SApplicative f, Known t, Indexed (f t), Fin t ~ Ix (f t)) => IFunctor ((f ∈ SApplicative) t) where
  imap = liftIA

instance (SApplicative f, Known t, Indexed (f t), Fin t ~ Ix (f t)) => IApplicative ((f ∈ SApplicative) t) where
  ipure :: forall a. (Fin t -> a) -> (f ∈ SApplicative) t a
  ipure = coerce do spure @_ @f @t @a sing

  liftIA2 :: forall a b c. (Fin t -> a -> b -> c) -> (f ∈ SApplicative) t a -> (f ∈ SApplicative) t b -> (f ∈ SApplicative) t c
  liftIA2 = coerce do liftSA2 @_ @f @t @a @b @c sing
