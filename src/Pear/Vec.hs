{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-top-binds -Wno-unused-imports #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE ConstrainedClassMethods #-}
-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE ViewPatterns #-}
module Pear.Vec 
  ( Vec(..)
  -- , shiftl, shiftr
  -- , rotl, rotr
  , Dict(..)
  )
  where

import Pear.Binary
import Pear.Singleton.Applicative
import Pear.Singleton.Known
import Pear.Finite
import Pear.Bit.Finite
import Pear.Bit.Singleton
import Pear.Binary.Singleton
import Pear.Binary.Finite
import Pear.Opt
import Pear.Two
import Pear.Indexed
import Pear.Via.Deindexed
import Pear.Via.Elem

import Data.Coerce
import Data.Functor ((<&>))
import Control.Monad.State
import Prelude hiding ((!!), reverse, succ, pred, (++))
import Control.Applicative (liftA2)
import Control.Applicative.Backwards
import Data.Tuple (swap)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import GHC.Types (Type, Constraint)

type Dict :: Constraint -> Type
data Dict c where
  Dict :: c => Dict c

data Unary = UZero | USucc Unary

type BZero :: Binary -> Type
data BZero n where
  BOb :: BZero 'Ob
  B_O :: BZero bs -> BZero (bs ':. 'O)

-- | a length-indexed collection
type Vec :: Unary -> Binary -> Type -> Type
data Vec u (length :: Binary) a where
  Nil :: BZero bs -> Vec 'UZero bs a
  Init :: BZero bs -> a -> Vec ('USucc 'UZero) (bs ':. 'I) a
  (:&) :: Vec ('USucc u) bs (Two a) -> Opt b a -> Vec ('USucc ('USucc (UAdjust b u))) (bs ':. b) a
infixl 4 :&

type UAdjust :: Bit -> Unary -> Unary
type family UAdjust b u where
  UAdjust 'O u = UDouble u
  UAdjust 'I u = 'USucc (UDouble u)

type UDouble :: Unary -> Unary
type family UDouble u = w | w -> u where
  UDouble 'UZero = 'UZero
  UDouble ('USucc u) = 'USucc ('USucc (UDouble u))

instance Functor (Vec u n) where
  fmap = undefined

instance Foldable (Vec u n) where
  foldMap = undefined

instance Traversable (Vec u n) where
  traverse = undefined

type Succ :: Binary -> Binary
type family Succ n where
  Succ (bs ':. 'I) = Succ bs ':. 'O
  Succ (bs ':. 'O) = PadZero bs ':. 'I
  Succ 'Ob = 'Ob ':. 'I

type PadZero :: Binary -> Binary
type family PadZero n where
  PadZero (bs ':. 'O) = PadZero bs ':. 'O
  PadZero (bs ':. 'I) = bs ':. 'I
  PadZero 'Ob = 'Ob ':. 'O

type Pred :: Binary -> Binary
type family Pred n where
  Pred (bs ':. 'O) = Pred bs ':. 'I
  Pred (bs ':. 'I) = PadNonZero bs
  Pred 'Ob = TypeError ('Text "undeflow")

type PadNonZero :: Binary -> Binary
type family PadNonZero n where
  PadNonZero (bs ':. 'O) = PadNonZero bs ':. 'O
  PadNonZero (bs ':. 'I) = bs ':. 'I ':. 'O
  PadNonZero 'Ob = 'Ob

push :: Vec u n a -> a -> Vec ('USucc u) (Succ n) a
push (vec :& Some a0) a1 = push vec (a0 :* a1) :& None
push (vec :& None) a = padZero vec :& Some a
push (Nil z) a = case z of
  BOb -> Init z a
  B_O z -> Init (padZero' z) a
push (Init z a0) a1 = case z of
  BOb -> Init z (a0 :* a1) :& None
  B_O z -> Init (padZero' z) (a0 :* a1) :& None

padZero :: Vec ('USucc u) n a -> Vec ('USucc u) (PadZero n) a
padZero (vec :& None) = padZero vec :& None
padZero (vec :& opt@(Some _)) = vec :& opt
padZero vec@(Init _ _) = vec

padZero' :: BZero n -> BZero (PadZero n)
padZero' BOb = B_O BOb
padZero' (B_O z) = B_O (padZero' z)

pop :: Vec ('USucc u) n a -> (Vec u (Pred n) a, a)
pop (vec :& None) = case pop vec of
  (Nil z, a0 :* a1) -> (Init z a0, a1)
  (vec@(Init _ _), a0 :* a1) -> (vec :& Some a0, a1)
  (vec@(_ :& _), a0 :* a1) -> (vec :& Some a0, a1)
pop (vec :& Some a) = (padNonZero (vec :& None), a)
pop (Init z a) = (Nil (padNonZero' z), a)

padNonZero :: Vec ('USucc u) (bs ':. 'O) a -> Vec ('USucc u) (PadNonZero bs) a
padNonZero (vec :& None) = case vec of
  _ :& None -> padNonZero vec :& None
  _ :& Some _ -> vec :& None
  Init _ _ -> vec :& None

padNonZero' :: BZero n -> BZero (PadNonZero n)
padNonZero' BOb = BOb
padNonZero' (B_O z) = B_O (padNonZero' z)

unshift :: a -> Vec u n a -> Vec ('USucc u) (Succ n) a
unshift a vec = uncurry push do traverse exchange vec `runState` a

shift :: Vec ('USucc u) n a -> (a, Vec u (Pred n) a)
shift vec = case pop vec of
  (vec, a) -> swap do forwards (traverse (Backwards . exchange) vec) `runState` a

{-# ANN exchange "HLint: ignore Use tuple-section" #-}
exchange :: a -> State a a
exchange orig = state \new -> (new, orig)

  {-
(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(Nil :& Some a) ++ vec' = Nil ++ unshift a vec'
(vec@(_ :& _) :& Some a) ++ vec' = (vec :& None) ++ unshift a vec'
(vec :& None) ++ (vec' :& opt) = case vec ++ vec' of
  vec@(_ :& _) -> vec :& opt
  Nil -> case opt of
    None -> Nil
    Some _ -> Nil :& opt

type (+) :: Binary -> Binary -> Binary
type family (+) n m where
  'Ob + m = m
  n + 'Ob = n
  (n ':. 'I) + m = Pred n 'I + Succ m
  (n ':. 'O) + (m ':. b) = (n + m) ':. b
  -}

{-
shiftr :: a -> Vec n a -> (Vec n a, a)
shiftr a vec = 
  do traverse exchange vec `runState` a
  
shiftl :: Vec n a -> a -> (a, Vec n a)
shiftl vec a = swap
  do forwards (traverse (Backwards . exchange) vec) `runState` a
          
{-# ANN exchange "HLint: ignore Use tuple-section" #-}
exchange :: a -> State a a
exchange orig = state \new -> (new, orig)

rotr :: Vec n a -> Vec n a
rotr vec = vec' where
  (vec', a) = shiftr a vec

rotl :: Vec n a -> Vec n a
rotl vec = vec' where
  (a, vec') = shiftl vec a
  -}

{-
step :: Vec m a -> Vec (Succ m) a -> (Vec (Succ m) a, Vec m a)
step Nil (Nil :& Some a) = (Nil :& Some a, Nil)
step (vec :& None) (vec' :& Some a) = (vec :& Some a, vec' :& None)
step (vec :& Some a) (vec' :& None) = case step vec vec' of
  (vec, vec') -> (vec :& None, vec' :& Some a)

step' :: Vec m a -> a -> Vec (Succ m) a -> (Vec (Succ m) a, Vec m a, a)
step' Nil a0 (Nil :& Some a1) = (Nil :& Some a0, Nil, a1)
step' (vec :& None) a0 (vec' :& Some a1) = (vec :& Some a0, vec' :& None, a1)
step' (vec :& Some a0) a1 (vec' :& None) = case step' vec (a0 :* a1) vec' of
  (vec, vec', a0 :* a1) -> (vec :& None, vec' :& Some a0, a1)
-}

{-
(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(++) = undefined

split :: SBinary n ->  Vec (n + m) a -> (Vec n a, Vec m a)
split = undefined
-}

-- TODO: 
--
-- >>> Nil
-- unsafeFromList []
-- >>> Nil :& Some a0
-- unsafeFromList [a0]
-- >>> Nil :& Some (Two a0 a1) :& Some a2
-- unsafeFromList [a0,a1,a2]
--
{-
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
-}
