{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Pear.Vec where

import Pear.Binary
import Pear.Singleton.Applicative
import Pear.Singleton.Known
import Pear.Finite
import Pear.Bit.Finite
import Pear.Binary.Singleton
import Pear.Binary.Finite
import Pear.Opt
import Pear.Two
import Pear.Indexed
import Pear.Via.Deindexed
import Pear.Via.Elem

import Data.Functor ((<&>))
import Prelude hiding ((!!), reverse)
import Control.Applicative (liftA2)

infixl 4 :&

type Vec :: Binary -> * -> *
data Vec (length :: Binary) a where
  Nil :: Vec 'Ob a
  (:&) :: { getForest :: !(Vec bs (Two a)), getSubtree :: !(Opt b a) } -> Vec (bs ':. b) a

deriving instance Show a => Show (Vec m a)
deriving instance Eq a => Eq (Vec m a)
deriving instance Ord a => Ord (Vec m a)

instance Indexed (Vec n) where
  type Ix (Vec n) = Fin n
  at Top f (vec :& Some a) = f a <&> \a -> vec :& Some a
  at (ix :! b) f (vec :& opt) = at ix (at b f) vec <&> \vec -> vec :& opt

deriving instance Functor (Vec m)
deriving instance Foldable (Vec m)
deriving instance Traversable (Vec m)
deriving via Deindexed (Vec m) instance Known m => Applicative (Vec m)
deriving via Deindexed (Vec m) instance Known m => Monad (Vec m)

deriving via (Vec m ∈ ITraversable) instance IFunctor (Vec m)
deriving via (Vec m ∈ ITraversable) instance IFoldable (Vec m)

instance ITraversable (Vec m) where
  itraverse _ Nil = pure Nil
  itraverse f (veca :& opta) = liftA2 (:&) 
    do itraverse (\ix -> itraverse \b -> f (ix :! b)) veca
    do itraverse (\Z -> f Top) opta

instance SApplicative Vec where
  spure SOb _ = Nil
  spure (ts ::. t) f = spure ts (\ix -> ipure (f . (ix :!))) :& spure t (\Z -> f Top)

  liftSA2 SOb _ Nil Nil = Nil
  liftSA2 (ts ::. t) f (veca :& opta) (vecb :& optb) = (:&)
    do liftSA2 ts (\ix a b -> (f (ix :! O) :* f (ix :! I)) <*> a <*> b) veca vecb
    do liftSA2 t (\Z -> f Top) opta optb

deriving via (Vec ∈ SApplicative) m instance Known m => IApplicative (Vec m)

instance Known m => IMonad (Vec m) where
  ibind f = imap \ix a -> f ix a !! ix
