{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Pear.Singleton.Applicative where

import GHC.Types (Constraint)
import Pear.Singleton
import Pear.Singleton.Known
import Pear.Via.Elem
import Pear.Finite
import Pear.Indexed

type SApplicative :: (k -> * -> *) -> Constraint
class SApplicative (f :: k -> * -> *) where
  spure :: Sing t -> (Fin t -> a) -> f t a
  liftSA2 :: Sing t -> (Fin t -> a -> b -> c) -> f t a -> f t b -> f t c

instance (SApplicative f, Known t) => Functor ((f ∈ SApplicative) t) where
  fmap = undefined

instance (SApplicative f, Known t) => Applicative ((f ∈ SApplicative) t) where
  pure = undefined
  (<*>) = undefined

instance (SApplicative f, Known t) => Indexed ((f ∈ SApplicative) t) where
  type Ix ((f ∈ SApplicative) t) = Ix (f t)
  at = undefined
  
instance (SApplicative f, Known t) => IFunctor ((f ∈ SApplicative) t) where
  imap = undefined

instance (SApplicative f, Known t) => IApplicative ((f ∈ SApplicative) t) where
  ipure = undefined
  liftIA2 = undefined
