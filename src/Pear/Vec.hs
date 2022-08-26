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
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE ViewPatterns #-}
module Pear.Vec 
  ( Vec(..)
  , shiftl, shiftr
  , rotl, rotr
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
import Prelude hiding ((!!), reverse, succ, pred)
import Control.Applicative (liftA2)
import Control.Applicative.Backwards
import Data.Tuple (swap)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import GHC.Types (Type, Constraint)

-- | a length-indexed collection
type Vec :: Binary -> Type -> Type
data Vec (length :: Binary) a where
  Nil :: Vec 'Ob a
  (:&) :: { getForest :: !(Vec bs (Two a)), getSubtree :: !(Opt b a) } -> Vec (bs ':. b) a
infixl 4 :&

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

type Unary :: Type
data Unary = UZero | USucc Unary

type SUnary :: Unary -> Type
data SUnary n where
  SZero :: SUnary 'UZero
  SSucc :: SUnary u -> SUnary ('USucc u)

type BZero :: Binary -> Type
data BZero n where
  BOb :: BZero 'Ob
  B_O :: BZero n -> BZero (n ':. 'O)

type Equiv :: Binary -> Unary -> Type
data Equiv n u where
  EqZero  :: BZero n -> Equiv n 'UZero
  EqEven  :: Equiv bs ('USucc us) -> Equiv (bs ':. 'O) ('USucc ('USucc (UDouble us)))
  EqOdd   :: Equiv bs us -> Equiv (bs ':. 'I) ('USucc (UDouble us))

toSBinary' :: BZero n -> SBinary n
toSBinary' BOb = SOb
toSBinary' (B_O z) = toSBinary' z ::. SO

toSBinary :: Equiv n u -> SBinary n
toSBinary = \case
  EqZero z  -> toSBinary' z
  EqEven e  -> toSBinary e ::. SO
  EqOdd e   -> toSBinary e ::. SI

toSUnary :: Equiv n u -> SUnary u
toSUnary = \case
  EqZero _  -> SZero
  EqEven e  -> udouble (toSUnary e)
  EqOdd e   -> SSucc (udouble (toSUnary e))

udouble :: SUnary u -> SUnary (UDouble u)
udouble SZero = SZero
udouble (SSucc u) = SSucc (SSucc (udouble u))

fromSBinary :: SBinary n -> (forall u. Equiv n u -> r) -> r
fromSBinary SOb f = f (EqZero BOb)
fromSBinary (ts ::. SO) f = fromSBinary ts \case
  EqZero z -> f do EqZero (B_O z)
  e@(EqEven _)  -> f do EqEven e
  e@(EqOdd _)   -> f do EqEven e
fromSBinary (ts ::. SI) f = fromSBinary ts \case
  e@(EqZero _)  -> f do EqOdd e
  e@(EqEven _)  -> f do EqOdd e
  e@(EqOdd _)   -> f do EqOdd e

data UEquiv u where
  UEquiv :: Equiv n u -> UEquiv u

umap :: (forall m r. (forall n. Equiv n v -> r) -> Equiv m u -> r) -> UEquiv u -> UEquiv v
umap f (UEquiv e) = f UEquiv e

-- multiple non-canonical encodings
fromSUnary :: SUnary u -> [UEquiv u]
fromSUnary SZero = iterate 
  do umap (.exten)
  do UEquiv (EqZero BOb)
fromSUnary (SSucc su) = umap (.incr) <$> fromSUnary su

incr :: Equiv n u -> Equiv (Incr n) ('USucc u)
incr (EqOdd e) = EqEven (incr e)
incr (EqEven e) = case _PadNonZero e of Dict -> EqOdd e
incr e@(EqZero _) = case _IncrZero e of Dict -> EqOdd e

decr :: Equiv n ('USucc u) -> Equiv (Decr n) u
decr (EqEven e) = EqOdd (decr e)
decr (EqOdd e@(EqZero _)) = case _UnpadZero e of Dict -> e
decr (EqOdd e@(EqOdd _)) = EqEven e
decr (EqOdd e@(EqEven x)) = case _UnpadNonZero x of Dict -> EqEven e

push :: forall n u a. Equiv n u -> Vec n a -> a -> (Equiv (Incr n) ('USucc u), Vec (Incr n) a)
push (EqOdd e) (vec :& Some a0) a1 = case push e vec (a0 :* a1) of
  ~(e, vec) -> (EqEven e, vec :& None)
push (EqEven e) (vec :& None) a = case pad e vec of
  ~(e, vec) -> (EqOdd e, vec :& Some a)
push e@(EqZero _) vec a = case _IncrZero e of 
  Dict -> (EqOdd e, mapEmpty e vec :& Some a)

-- XXX: should just be a coerce
mapEmpty :: Equiv n 'UZero -> Vec n a -> Vec n b
mapEmpty (EqZero BOb) Nil = Nil
mapEmpty (EqZero (B_O z)) (vec :& None) = mapEmpty (EqZero z) vec :& None

_IncrZero :: Equiv n 'UZero -> Dict (Incr n ~ (n ':. 'I))
_IncrZero (EqZero BOb) = Dict
_IncrZero e@(EqZero (B_O _)) = case _PadZero (trunc e) of Dict -> Dict

trunc :: Equiv (n ':. 'O) 'UZero -> Equiv n 'UZero
trunc (EqZero (B_O z)) = EqZero z

exten :: Equiv n 'UZero -> Equiv (n ':. 'O) 'UZero
exten (EqZero z) = EqZero (B_O z)

_PadZero :: Equiv n 'UZero -> Dict (Pad n ~ (n ':. 'O))
_PadZero e@(EqZero (B_O _)) = case _PadZero (trunc e) of Dict -> Dict
_PadZero (EqZero BOb) = Dict

_PadNonZero :: Equiv n ('USucc u) -> Dict (Pad n ~ n)
_PadNonZero (EqEven e) = case _PadNonZero e of Dict -> Dict
_PadNonZero (EqOdd _) = Dict

pad :: Equiv n ('USucc u) -> Vec n (Two a) -> (Equiv (Pad n) ('USucc u), Vec (Pad n) (Two a))
pad (EqEven e) (vec :& None) = case pad e vec of 
  ~(e, vec) -> (EqEven e, vec :& None)
pad e@(EqOdd _) vec@(_ :& Some _) = (e, vec) 

_PopPush :: Equiv n u -> Vec n a -> a -> (Equiv n u, Vec n a, a)
_PopPush e vec a = case _DecrIncr (toSBinary e) of
  Dict -> uncurry pop do push e vec a

pop :: Equiv n ('USucc u) -> Vec n a -> (Equiv (Decr n) u, Vec (Decr n) a, a)
pop (EqEven e) (vec :& None) = case pop e vec of
  ~(e, vec, a0 :* a1) ->
    ( case e of
        e@(EqZero _)  -> EqOdd e
        e@(EqEven _)  -> EqOdd e
        e@(EqOdd _)   -> EqOdd e
    , vec :& Some a0
    , a1
    )
pop (EqOdd e) (vec :& Some a) = case unpad e vec of
  ~(e, vec) -> (e, vec, a)

unpad :: Equiv n u -> Vec n (Two a) -> (Equiv (Unpad n) (UDouble u), Vec (Unpad n) a)
unpad (EqEven e) (vec :& None) = case unpad e vec of
  ~(e, vec) -> (EqEven e, vec :& None)
unpad e@(EqOdd _) vec@(_ :& _) = (EqEven e, vec :& None)
unpad e@(EqZero _) vec = case _UnpadZero e of
  Dict -> (e, mapEmpty e vec)

--- XXX : Unpad is a bad name, since it's not the inverse of Pad
_UnpadZero :: Equiv n 'UZero -> Dict (Unpad n ~ n)
_UnpadZero e@(EqZero (B_O _)) = case _UnpadZero (trunc e) of Dict -> Dict
_UnpadZero (EqZero BOb) = Dict

_UnpadNonZero :: Equiv n ('USucc u) -> Dict (Unpad n ~ (n ':. 'O))
_UnpadNonZero (EqEven e) = case _UnpadNonZero e of Dict -> Dict
_UnpadNonZero (EqOdd _) = Dict

_DecrIncr :: SBinary n -> Dict (Decr (Incr n) ~ n)
_DecrIncr (ts ::. SI) = case _DecrIncr ts of Dict -> Dict
_DecrIncr (ts ::. SO) = case _UnpadPad ts of Dict -> Dict
_DecrIncr SOb = Dict

_UnpadPad :: SBinary n -> Dict (Unpad (Pad n) ~ (n ':. 'O))
_UnpadPad (ts ::. SO) = case _UnpadPad ts of Dict -> Dict
_UnpadPad (__ ::. SI) = Dict
_UnpadPad SOb = Dict

type Decr :: Binary -> Binary
type family Decr n where
  Decr (bs ':. 'O) = Decr bs ':. 'I
  Decr (bs ':. 'I) = Unpad bs
  Decr 'Ob = TypeError ('Text "type error: underflow")

type Unpad :: Binary -> Binary
type family Unpad n where
  Unpad (bs ':. 'O) = Unpad bs ':. 'O
  Unpad (bs ':. 'I) = bs ':. 'I ':. 'O
  Unpad 'Ob = 'Ob

type Incr :: Binary -> Binary
type family Incr n where
  Incr (bs ':. 'I) = Incr bs ':. 'O
  Incr (bs ':. 'O) = Pad bs ':. 'I
  Incr 'Ob = 'Ob ':. 'I

type Pad :: Binary -> Binary
type family Pad n where
  Pad (bs ':. 'O) = Pad bs ':. 'O
  Pad (bs ':. 'I) = bs ':. 'I -- canonical
  Pad 'Ob = 'Ob ':. 'O -- preserve non-canonical padding

type UDouble :: Unary -> Unary
type family UDouble u = w | w -> u where
  UDouble 'UZero = 'UZero
  UDouble ('USucc u) = 'USucc ('USucc (UDouble u))

type Dict :: Constraint -> Type
data Dict c where
  Dict :: c => Dict c
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
